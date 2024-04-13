namespace SoulyaBoy.Core

open System.Runtime.CompilerServices

type SBGpuMode =
    | HBlank = 0
    | VBlank = 1
    | OAM = 2
    | Draw = 3

[<Struct; IsReadOnly>]
type OBJDesc = {
    RY: byte
    RX: byte
    TileId: byte
    Flags: byte
}

[<Struct; IsReadOnly>]
type SBGpuDrawCall =
    | OBJ of OBJDesc
    | Tile

[<Struct; IsReadOnly>]
type SBGpu =
    { LY: byte
      LYC: byte
      LCDC: byte
      STAT: byte
      BGF: byte
      OBP0: byte
      OBP1: byte
      SCX: byte
      SCY: byte
      DMA: byte
      DMATransfer: bool
      DrawCalls: array<SBGpuDrawCall>
      Dots: uint32
      Mode: SBGpuMode }

module SBGpuFactory =
    let CreateGPU =
        { LY = 0uy
          LYC = 0uy
          LCDC = 0x91uy
          STAT = 0x81uy
          BGF = 0xFCuy
          OBP0 = 0uy
          OBP1 = 0uy 
          SCX = 0uy
          SCY = 0uy
          DMA = 0uy
          DMATransfer = false
          DrawCalls = Array.create 160 SBGpuDrawCall.Tile
          Dots = 0u
          Mode = SBGpuMode.OAM }
