namespace SoulyaBoy.Core

module SBGraphics = 

    let private sb = new SBBuilder()

    let DrawTile pixelPipe tileId tileIndex = sb {
        for i in 0us..8us do
            let address = 0x8000us + uint16(tileId) * 16us + 2us*i;
            let! row1 = SBIO.ReadByte (address)
            let! row2 = SBIO.ReadByte (address+1us)

            for j in 0..8 do 
                let shade = (row1 >>> j &&& 1uy) ||| (row2 >>> j &&& 1uy) <<< 1;
                let px = 8 * (int tileIndex % 32) + j
                let py = 8 * (int tileIndex / 32) + int i
                pixelPipe px py shade
    }

    let Process pixelPipe = sb {
        let! mb = SB.Get
        let LY = if mb.GPU.LY < 153uy then mb.GPU.LY + 1uy else 0uy
        do! SBIO.WriteByte 0xFF44us LY

        for i in 0x9800us..0x9BFFus do
            let! tileId = SBIO.ReadByte i
            do! DrawTile pixelPipe tileId (i - 0x9800us)
    }
