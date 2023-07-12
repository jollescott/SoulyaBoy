namespace SoulyaBoy.Core

module SBGraphics = 

    let private sb = new SBBuilder()

    let Process = sb {
        let! mb = SB.Get
        let LY = if mb.GPU.LY < 153uy then mb.GPU.LY + 1uy else 0uy
        do! SBIO.WriteByte 0xFF44us LY


    }
