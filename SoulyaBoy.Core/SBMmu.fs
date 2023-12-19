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
      HRAM: array<byte> }


module SBMmuFactory =
    let internal CreateMMU (rom: byte array) =

        let io = Array.zeroCreate 128
        
        let seedIO address value = 
          io[address - 0xFF00] <- value

        seedIO 0xFF00 0xCFuy
        seedIO 0xFF10 0x80uy
        seedIO 0xFF11 0xBFuy
        seedIO 0xFF12 0xF3uy
        seedIO 0xFF14 0xBFuy
        seedIO 0xFF16 0x3Fuy
        seedIO 0xFF19 0xBFuy
        seedIO 0xFF1A 0x7Fuy
        seedIO 0xFF1B 0xFFuy
        seedIO 0xFF1C 0x9Fuy
        seedIO 0xFF1E 0xBFuy
        seedIO 0xFF20 0xFFuy
        seedIO 0xFF23 0xBFuy
        seedIO 0xFF24 0x77uy
        seedIO 0xFF25 0xF3uy
        seedIO 0xFF26 0xF1uy
        seedIO 0xFF47 0xFCuy
        seedIO 0xFF48 0xFFuy
        seedIO 0xFF49 0xFFuy

        { ROM = rom
          VRAM = Array.zeroCreate 8192
          WRAM = Array.zeroCreate 8192
          EXRAM = Array.zeroCreate 8192
          OAM = Array.zeroCreate 160
          UNUSABLE = Array.zeroCreate 96
          IO = io
          HRAM = Array.zeroCreate 128 }
