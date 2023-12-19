namespace SoulyaBoy.Core

module SBGraphics = 

    let private sb = SBBuilder()

    let DrawPixel pixelPipe dot ly = sb {
        let! mb = SB.Get

        let TILE_MAP_BASE = if (mb.GPU.LCDC &&& 0b1000uy) >>> 3 = 1uy then 0x9C00us else 0x9800us

        let tileIndex = (uint16 ly / 8us) * 32us + (uint16 dot % 160us) / 8us
        let! tileId = SBIO.ReadByte (TILE_MAP_BASE + tileIndex)

        let sx = int dot % 160
        let sy = int ly

        let tx = sx % 8
        let ty = sy % 8

        let addressingMode = (mb.GPU.LCDC &&& 0b1_0000uy) >>> 4

        let address = if addressingMode = 1uy && tileId < 128uy then
                            0x8000us + (uint16 tileId - 1us) * 16us + (uint16 ty * 2us)
                        else if addressingMode <> 1uy && tileId < 128uy then
                            0x9000us + (uint16 tileId - 1us) * 16us + (uint16 ty * 2us)
                        else
                            0x8800us + (uint16 tileId - 127us) * 16us + (uint16 ty * 2us)
                            

        let! row1 = SBIO.ReadByte (address)
        let! row2 = SBIO.ReadByte (address+1us)

        let baseShade = (row1 >>> (7 - tx)) &&& 1uy ||| ((row2 >>> (7 - tx)) <<< 1) &&& 1uy;
        let paletteShade = (mb.GPU.BGF >>> 2 * int baseShade) &&& 0b11uy

        pixelPipe sx sy paletteShade
    }
    
    let QueueVBlank = sb {
        let! mb = SB.Get
        do! SB.Put { mb with CPU = {mb.CPU with IF = 1uy } }
    }

    let UpdateGPUMode mode = sb {
        let! mb = SB.Get
        let stat = (0b1111_1100uy &&& mb.GPU.STAT) ||| (0b11uy &&& byte mode)
        do! SB.Put { mb with GPU = {mb.GPU with Mode = mode; STAT = stat }}
    }
    
    let UpdateGPUState dots ly = sb {
        let! mb = SB.Get
        do! SB.Put {mb with GPU = { mb.GPU with Dots = dots + 4u; LY = ly }}
    }

    let UpdateLYC ly = sb {
        let! mb = SB.Get

        if (mb.GPU.STAT >>> 2) <> 1uy && mb.GPU.LYC = ly then
            do! SB.Put { mb with GPU = { mb.GPU with STAT = mb.GPU.STAT ||| 0b100uy }}
        else if (mb.GPU.STAT >>> 2) = 1uy && mb.GPU.LYC <> ly then
            do! SB.Put { mb with GPU = { mb.GPU with STAT = (mb.GPU.STAT &&& 0b1111_1011uy) ||| 0b011uy }}
    }
    
    let Process pixelPipe = sb {
        let! mb = SB.Get 

        if mb.GPU.LCDC >>> 7 = 1uy then
            let dots = mb.GPU.Dots
        
            let LY = if mb.GPU.LY = 154uy then 0uy else if dots % 456u = 0u then mb.GPU.LY + 1uy else mb.GPU.LY
        
            let lineDots = dots % 456u

            if LY = 144uy && mb.GPU.Mode = SBGpuMode.HBlank then
                do! UpdateGPUMode SBGpuMode.VBlank
            else if (lineDots = 0u && mb.GPU.Mode = SBGpuMode.HBlank) || (mb.GPU.Mode = SBGpuMode.VBlank && LY = 0uy) then
                do! UpdateGPUMode SBGpuMode.OAM
            else if lineDots = 80u && mb.GPU.Mode = SBGpuMode.OAM then
                do! UpdateGPUMode SBGpuMode.Draw
            else if lineDots = 252u && mb.GPU.Mode = SBGpuMode.Draw then
                do! UpdateGPUMode SBGpuMode.HBlank
        
            if lineDots < 240u && LY < 144uy then
                do! DrawPixel pixelPipe dots LY
                do! DrawPixel pixelPipe (dots+1u) LY
                do! DrawPixel pixelPipe (dots+2u) LY
                do! DrawPixel pixelPipe (dots+3u) LY

            do! UpdateLYC LY
            do! UpdateGPUState dots LY            
    }
