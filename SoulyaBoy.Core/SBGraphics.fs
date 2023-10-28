namespace SoulyaBoy.Core

module SBGraphics = 

    let private sb = new SBBuilder()

    let DrawTile pixelPipe tileId tileIndex = sb {
        let! mb = SB.Get

        let TILE_DATA_START = if(mb.GPU.LCDC &&& 0b10000uy) = 1uy then 0x8000us else 0x8800us
        
        for i in 0us..7us do
            let address = TILE_DATA_START + uint16(tileId) * 16us + 2us*i;
            let! row1 = SBIO.ReadByte (address)
            let! row2 = SBIO.ReadByte (address+1us)

            for j in 0..7 do 
                let shade = (row1 >>> j &&& 1uy) ||| (row2 >>> j &&& 1uy) <<< 1;
                let px = 8 * (int tileIndex % 32) + j
                let py = 8 * (int tileIndex / 32) + int i
                pixelPipe px py shade
    }

    let Process pixelPipe = sb {
        let! mb = SB.Get

        let LY = if mb.GPU.LY < 153uy then mb.GPU.LY + 1uy else 0uy
        do! SBIO.WriteByte 0xFF44us LY

        // TODO: This block is slow.
        if (LY = 0uy) then
            let TILE_MAP_START = if (mb.GPU.LCDC &&& 0b100uy) = 1uy then 0x9C00us else 0x9800us
            let TILE_MAP_END = if (mb.GPU.LCDC &&& 0b100uy) = 1uy then 0x9FFFus else 0x9BFFus

            for i in TILE_MAP_START..TILE_MAP_END do
                let! tileId = SBIO.ReadByte i
                do! DrawTile pixelPipe tileId (i - TILE_MAP_START)
    }
