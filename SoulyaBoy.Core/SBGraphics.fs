namespace SoulyaBoy.Core

module SBGraphics = 
    let Process sb = 
        let LY = MmuIO.ReadByte sb.MMU 0xFF44us
        MmuIO.WriteByte sb.MMU 0xFF44us (if LY < 153uy then LY + 1uy else 0uy)
        sb