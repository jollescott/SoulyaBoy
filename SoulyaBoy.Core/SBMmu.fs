namespace SoulyaBoy.Core

type SBMmu =
    { ROM: array<byte>
      // TODO: Switchable banks.
      VRAM: array<byte>
      WRAM: array<byte>
      EXRAM: array<byte>
      OAM: array<byte>
      UNUSABLE: array<byte>
      IO: array<byte>
      HRAM: array<byte>
      IE: array<byte> }


module SBMmuFactory =
    let internal CreateMMU (rom: byte array) =
        { ROM = rom
          VRAM = Array.zeroCreate 8192
          WRAM = Array.zeroCreate 8192
          EXRAM = Array.zeroCreate 8192
          OAM = Array.zeroCreate 160
          UNUSABLE = Array.zeroCreate 96
          IO = Array.zeroCreate 128
          HRAM = Array.zeroCreate 128
          IE = Array.zeroCreate 1 }
