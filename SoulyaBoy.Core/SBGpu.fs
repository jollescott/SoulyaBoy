namespace SoulyaBoy.Core

open System.Runtime.CompilerServices

type SBGpuMode = 
    | HBlank = 0
    | VBlank = 1
    | OAM = 2
    | Draw = 3

[<Struct; IsReadOnly>]
type SBGpu = {
    LY: byte
    LYC: byte
    LCDC: byte
    STAT: byte
    BGF: byte
    SCX: byte
    SCY: byte
    Dots: uint32 
    Mode: SBGpuMode
}

module SBGpuFactory = 
    let CreateGPU = 
        { LY = 0x91uy
          LYC = 0uy
          LCDC = 0x91uy
          STAT = 0x81uy
          BGF = 0xFCuy
          SCX = 0uy
          SCY = 0uy
          Dots = 0u
          Mode = SBGpuMode.OAM }

