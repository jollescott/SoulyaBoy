namespace SoulyaBoy.Core

module SBGraphics = 

    let private sb = SBBuilder()

    let DrawTile pixelPipe tileId tileIndex ly = sb {
        let! mb = SB.Get

        let TILE_DATA_START = if(mb.GPU.LCDC &&& 0b10000uy) = 1uy then 0x8000us else 0x8800us
        
        let address = TILE_DATA_START + uint16(tileId) * 16us + 2us * (uint16 ly % 8us);
        let! row1 = SBIO.ReadByte (address)
        let! row2 = SBIO.ReadByte (address+1us)

        for j in 0..7 do 
            let shade = (row1 >>> j &&& 1uy) ||| ((row2 >>> j &&& 1uy) <<< 1);
            let px = 8 * (int tileIndex % 32) + j
            let py = int ly
            pixelPipe px py shade
    }

    let Process pixelPipe = sb {
        let! mb = SB.Get 

        let LY = if mb.GPU.LY < 255uy then mb.GPU.LY + 1uy else 0uy
        do! SBIO.WriteByte 0xFF44us LY

        let TILE_MAP_BASE_START = if (mb.GPU.LCDC &&& 0b100uy) = 1uy then 0x9C00us else 0x9800us
        let TILE_MAP_BASE_END = if (mb.GPU.LCDC &&& 0b100uy) = 1uy then 0x9FFFus else 0x9BFFus

        let TILE_MAP_START = TILE_MAP_BASE_START + (uint16 LY) * 32us
        let TILE_MAP_END = if(TILE_MAP_BASE_START <= TILE_MAP_BASE_END - 32us) then TILE_MAP_START + 32us else TILE_MAP_BASE_END

        for tileMapAddress in TILE_MAP_START..TILE_MAP_END do
            let! tileId = SBIO.ReadByte tileMapAddress
            do! DrawTile pixelPipe tileId (tileMapAddress - TILE_MAP_BASE_START) LY
    }
