namespace SoulyaBoy.Core

open System.Runtime.CompilerServices

[<Struct; IsReadOnly>]
type SBGpu = {
    LY: byte
    LCDC: byte
    Dots: uint32 
}

module SBGpuFactory = 
    let CreateGPU = 
        { LY = 0uy 
          LCDC = 0x91uy
          Dots = 0u }

