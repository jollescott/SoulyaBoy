namespace SoulyaBoy.Core

module SBGraphics = 
    let Process mb = 
        mb |> SBIO.WriteByte 0xFF44us (if mb.GPU.LY < 153uy then mb.GPU.LY + 1uy else 0uy)
