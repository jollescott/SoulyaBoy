namespace SoulyaBoy.Core

module SBGraphics = 

    let private sb = new SBBuilder()

    let rec readTileMap list address tileMapMax tileDataPrefix = sb {
        let! tileDataIndex = SBIO.ReadByte address

        let value = SBIO.ReadShort (uint16 tileDataIndex + tileDataPrefix)
        let updatedList = value :: list

        if address = tileMapMax then return updatedList else return! readTileMap updatedList (address + 1us) tileMapMax tileDataPrefix
    }

    let Process = sb {
        let! mb = SB.Get
        let LY = if mb.GPU.LY < 153uy then mb.GPU.LY + 1uy else 0uy
        do! SBIO.WriteByte 0xFF44us LY

        // Select the background tile map range
        let (bgTileMapMin, bgTileMapMax) = if mb.GPU.LCDC &&& 0b1000uy <> 0uy then (0x9800us, 0x9BFFus) else (0x9C00us, 0x9FFFus)

        let (bgTileDataMin, _) = if mb.GPU.LCDC &&& 0b10000uy <> 0uy then (0x8800us, 0x97FFus) else (0x8000us, 0x8FFFus)
        let! tileData = readTileMap [] bgTileMapMin bgTileMapMax bgTileDataMin
        return tileData
    }
