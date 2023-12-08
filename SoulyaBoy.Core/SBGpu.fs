namespace SoulyaBoy.Core

open System.Runtime.CompilerServices

[<Struct; IsReadOnly>]
type SBGpu = {
    LY: byte
    LCDC: byte
}

module SBGpuFactory = 
    let CreateGPU = 
        { LY = 0uy 
          LCDC = 0x91uy }

