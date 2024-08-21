namespace SoulyaBoy.Core

open System

type SBROM = 
    | Unloaded
    | Loaded of array<byte>

module SBIO =

    let internal sb = SBBuilder()
    let internal random = Random()

    let private initIO =

        let io = Array.zeroCreate 128

        let seedIO address value = io[address - 0xFF00] <- value

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

        io

    let mutable ROM = Unloaded
    let internal VRAM: array<byte> = Array.zeroCreate 8192
    let internal WRAM: array<byte> = Array.zeroCreate 8192
    let internal EXRAM: array<byte> = Array.zeroCreate 8192
    let internal OAM: array<byte> = Array.zeroCreate 160
    let internal UNUSABLE: array<byte> = Array.zeroCreate 96
    let internal IO: array<byte> = initIO
    let internal HRAM: array<byte> = Array.zeroCreate 128

    let ReadByte address =
        sb {
            let! mb = SB.Get

            return
                match address with
                | address when 0xffffus = address -> mb.CPU.IE
                | address when 0xff0fus = address -> mb.CPU.IF
                | address when 0xff80us <= address && address <= 0xfffeus -> HRAM[int address - 0xff80]
                | address when address = 0xFF47us -> mb.GPU.BGF
                | address when address = 0xFF48us -> mb.GPU.OBP0
                | address when address = 0xFF49us -> mb.GPU.OBP1
                | address when address = 0xFF46us -> mb.GPU.DMA
                | address when address = 0xFF45us -> mb.GPU.LYC
                | address when address = 0xFF44us -> mb.GPU.LY
                | address when address = 0xFF41us -> mb.GPU.STAT
                | address when address = 0xFF42us -> mb.GPU.SCY
                | address when address = 0xFF43us -> mb.GPU.SCX
                | address when address = 0xFF40us -> mb.GPU.LCDC
                | address when address = 0xFF04us -> byte (random.Next(255))
                | address when address = 0xFF00us -> mb.Joypad
                | address when 0xff00us <= address && address <= 0xff7fus -> IO[int address - 0xff00]
                | address when 0xfea0us <= address && address <= 0xfeffus -> UNUSABLE[int address - 0xfea0]
                | address when 0xfe00us <= address && address <= 0xfe9fus -> OAM[int address - 0xfe00]
                | address when 0xe000us <= address && address <= 0xfdffus -> WRAM[int address - 0xe000]
                | address when 0xc000us <= address && address <= 0xdfffus -> WRAM[int address - 0xc000]
                | address when 0xa000us <= address && address <= 0xbfffus -> EXRAM[int address - 0xa000]
                | address when 0x8000us <= address && address <= 0x9fffus -> VRAM[int address - 0x8000]
                | address when 0x0000us <= address && address <= 0x7fffus -> 
                    match ROM with
                    | Unloaded -> raise (Exception("ROM is unloaded!"))
                    | Loaded(rom) -> rom[int address]
                | _ -> raise (Exception("Failed to access " + address.ToString()))
        }

    let WriteByte address value =
        sb {
            let! mb = SB.Get

            let mmb =
                match address with
                | address when 0xffffus = address -> { mb with CPU.IE = value }
                | address when 0xff0fus = address -> { mb with CPU.IF = value }
                | address when 0xff80us <= address && address <= 0xfffeus -> HRAM[int address - 0xff80] <- value; mb
                | address when address = 0xFF47us -> { mb with GPU.BGF = value }
                | address when address = 0xFF48us -> { mb with GPU.OBP0 = value }
                | address when address = 0xFF49us -> { mb with GPU.OBP1 = value }
                | address when address = 0xFF46us -> { mb with GPU.DMA = value; GPU.DMATransfer = true }
                | address when address = 0xFF45us -> { mb with GPU.LYC = value }
                | address when address = 0xFF44us -> { mb with GPU.LY = value }
                | address when address = 0xFF41us -> { mb with GPU.STAT = value }
                | address when address = 0xFF42us -> { mb with GPU.SCY = value }
                | address when address = 0xFF43us -> { mb with GPU.SCX = value }
                | address when address = 0xFF40us -> { mb with GPU.LCDC = value }
                | address when address = 0xFF04us -> mb
                | address when address = 0xFF00us -> { mb with Joypad = value }
                | address when 0xff00us <= address && address <= 0xff7fus -> IO[int address - 0xff00] <- value; mb
                | address when 0xfea0us <= address && address <= 0xfeffus -> UNUSABLE[int address - 0xfea0] <- value; mb
                | address when 0xfe00us <= address && address <= 0xfe9fus -> OAM[int address - 0xfe00] <- value; mb
                | address when 0xe000us <= address && address <= 0xfdffus -> WRAM[int address - 0xe000] <- value; mb
                | address when 0xc000us <= address && address <= 0xdfffus -> WRAM[int address - 0xc000] <- value; mb
                | address when 0xa000us <= address && address <= 0xbfffus -> EXRAM[int address - 0xa000] <- value; mb
                | address when 0x8000us <= address && address <= 0x9fffus -> VRAM[int address - 0x8000] <- value; mb
                | address when 0x0000us <= address && address <= 0x7fffus -> mb
                | _ -> raise (Exception("Failed to access " + address.ToString()))

            do! SB.Put mmb
        }


    let ReadShort address =
        sb {
            let! low = ReadByte address
            let! high = ReadByte(address + 1us)

            return SBUtils.toShort high low
        }

    let WriteShort address value =
        sb {
            let struct (high, low) = SBUtils.toBytes value
            do! (WriteByte address low)
            do! (WriteByte (address + 1us) high)
        }
