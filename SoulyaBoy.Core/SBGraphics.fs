namespace SoulyaBoy.Core

module SBGraphics = 
    let Process sb = 
        sb |> SBIO.WriteByte 0xFF44us (if sb.GPU.LY < 153uy then sb.GPU.LY + 1uy else 0uy)
