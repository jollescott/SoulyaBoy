namespace SoulyaBoy.Core

module SBGraphics =

    let private sb = SBBuilder()

    let ProcessDMATransfer =
        sb {
            let! mb = SB.Get
            let source = (uint16 mb.GPU.DMA) * 0x100us

            for i = 0us to 0x9Fus do
                let! object = SBIO.ReadByte(source + i)
                do! SBIO.WriteByte (0xFE00us + i) object

            do! SB.Put { mb with GPU.DMATransfer = false }
        }

    let ScanOAM oamDots offset =
        sb {
            // TODO: Investigate why only running once without this line.
            let! mb = SB.Get
            
            let objAddress = 0xFE00us + (uint16 (2u * oamDots) + offset * 4us)
            
            let! objY = SBIO.ReadByte objAddress
            let overlapCheck = int mb.GPU.LY - (int objY - 16)
            
            let objSize = int (mb.GPU.LCDC &&& 0b100uy) >>> 2
                        
            if overlapCheck >= 0 && overlapCheck < 8*(objSize+1) then
                let! objX = SBIO.ReadByte(objAddress + 1us)

                if objX >= 8uy && objX < 160uy then
                    let! tileIndex = SBIO.ReadByte(objAddress + 2us)
                    let! attributes = SBIO.ReadByte(objAddress + 3us)
                                                                                                                                                   
                    for i = 0 to 7 do
                        let objDesc = { RY = byte overlapCheck; RX = byte i; TileId = tileIndex; Flags = attributes }
                        mb.GPU.DrawCalls[int objX - 8 + i] <- OBJ(objDesc)
        }
        
    let DrawTileSegment struct(sx, sy) struct(tx, ty) palette address pixelPipe = sb {            
            let! row1 = SBIO.ReadByte address
            let! row2 = SBIO.ReadByte(address + 1us)
            
            let colorId =
                ((row1 >>> (7 - tx)) &&& 1uy)
                ||| (((row2 >>> (7 - tx)) &&& 1uy) <<< 1)

            let paletteColor = (palette >>> 2 * int colorId) &&& 0b11uy

            pixelPipe sx sy paletteColor
        }

    let DrawOBJ pixelPipe dot desc =
        sb {
            let! mb = SB.Get
            let ly = mb.GPU.LY
                        
            let sx = int dot
            let sy = int ly
            
            let tx = int desc.RX
            let ty = int desc.RY
            
            let address = 0x8000us + (uint16 desc.TileId) * 16us + (uint16 ty * 2us)
            let palette = if desc.Flags &&& 0b1_0000uy <> 0uy then mb.GPU.OBP1 else mb.GPU.OBP0
            
            do! DrawTileSegment struct(sx, sy) struct(tx, ty) palette address pixelPipe
        }

    let DrawTile pixelPipe dot =
        sb {
            let! mb = SB.Get
            let ly = mb.GPU.LY

            let TILE_MAP_BASE =
                if (mb.GPU.LCDC &&& 0b1000uy) <> 0uy then
                    0x9C00us
                else
                    0x9800us

            let tileIndex = (uint16 ly / 8us) * 32us + uint16 dot / 8us
            let! tileId = SBIO.ReadByte(TILE_MAP_BASE + tileIndex)

            let sx = int dot
            let sy = int ly

            let addressingMode = (mb.GPU.LCDC &&& 0b1_0000uy) >>> 4

            let tx = sx % 8
            let ty = sy % 8
            
            let address =
                if addressingMode = 1uy && tileId < 128uy then
                    0x8000us
                    + (uint16 tileId) * 16us
                    + (uint16 ty * 2us)
                else if addressingMode <> 1uy && tileId < 128uy then
                    0x9000us
                    + (uint16 tileId) * 16us
                    + (uint16 ty * 2us)
                else
                    0x8800us
                    + (uint16 tileId - 128us) * 16us
                    + (uint16 ty * 2us)
                    
            do! DrawTileSegment struct(sx, sy) struct(tx, ty) mb.GPU.BGF address pixelPipe
        }

    let DrawPixel pixelPipe lineDots =
        sb {
            let! mb = SB.Get

            let drawDot = (lineDots - 80u) % 160u
            let drawCall = mb.GPU.DrawCalls[int drawDot]
                        
            match drawCall with
            | Tile -> do! DrawTile pixelPipe drawDot
            | OBJ objDesc -> do! DrawOBJ pixelPipe drawDot objDesc

            mb.GPU.DrawCalls[ int drawDot ] <- Tile
        }

    let QueueVBlank =
        sb {
            let! mb = SB.Get

            if mb.CPU.IE &&& 0b1uy <> 0uy then
                do! SB.Put { mb with CPU.IF = 1uy }
        }

    let UpdateGPUMode mode =
        sb {
            let! mb = SB.Get

            let stat =
                (0b1111_1100uy &&& mb.GPU.STAT)
                ||| (0b11uy &&& byte mode)
            
            do! SB.Put { mb with GPU = { mb.GPU with Mode = mode; STAT = stat } }
        }

    let UpdateGPUState =
        sb {
            let! mb = SB.Get

            let ly =
                if mb.GPU.Dots > 0u && mb.GPU.Dots % 456u = 0u then
                    if mb.GPU.LY = 153uy then 0uy else mb.GPU.LY + 1uy
                else
                    mb.GPU.LY
            
            let dots =
                if mb.GPU.Dots = 70224u then
                    0u
                else
                    mb.GPU.Dots + 4u
            
            do! SB.Put { mb with GPU = { mb.GPU with Dots = dots; LY = ly } }
        }

    let UpdateLYC ly =
        sb {
            let! mb = SB.Get

            if (mb.GPU.STAT >>> 2) <> 1uy && mb.GPU.LYC = ly then
                do! SB.Put { mb with GPU.STAT = mb.GPU.STAT ||| 0b100uy }
            else if (mb.GPU.STAT >>> 2) = 1uy && mb.GPU.LYC <> ly then
                do! SB.Put { mb with GPU.STAT = (mb.GPU.STAT &&& 0b1111_1011uy) ||| 0b011uy }
        }
    
    let Process pixelPipe =
        sb {
            let! mb = SB.Get
            
            if mb.GPU.LCDC >>> 7 = 1uy then
                let dots = mb.GPU.Dots
                let LY = mb.GPU.LY

                let lineDots = dots % 456u
                
                if LY = 143uy && mb.GPU.Mode = SBGpuMode.HBlank then
                    do! UpdateGPUMode SBGpuMode.VBlank
                    do! QueueVBlank
                else if (lineDots = 452u && mb.GPU.Mode = SBGpuMode.HBlank)
                        || (mb.GPU.Mode = SBGpuMode.VBlank && LY = 0uy) then
                    do! UpdateGPUMode SBGpuMode.OAM
                else if lineDots = 76u && mb.GPU.Mode = SBGpuMode.OAM then
                    do! UpdateGPUMode SBGpuMode.Draw
                else if lineDots = 236u && mb.GPU.Mode = SBGpuMode.Draw then
                    do! UpdateGPUMode SBGpuMode.HBlank

                if mb.GPU.Mode = SBGpuMode.OAM
                   && mb.GPU.LCDC &&& 0b10uy <> 0uy then
                    do! ScanOAM lineDots 0us
                    do! ScanOAM lineDots 1us
                else if mb.GPU.Mode = SBGpuMode.Draw && LY < 144uy then
                    do! DrawPixel pixelPipe lineDots
                    do! DrawPixel pixelPipe (lineDots + 1u) 
                    do! DrawPixel pixelPipe (lineDots + 2u)
                    do! DrawPixel pixelPipe (lineDots + 3u)

                if mb.GPU.DMATransfer then
                    do! ProcessDMATransfer
            
                do! UpdateLYC LY
                do! UpdateGPUState
        }
