namespace SoulyaBoy.Core

open System

module SBIO =

    let sb = SBBuilder()
    let random = Random()

    [<Struct>]
    type private IOAccess =
        | Array of struct (array<byte> * uint16)
        | IE
        | IF
        | BGF
        | OBP0
        | OBP1
        | LYC
        | LY
        | STAT
        | SCY
        | SCX
        | DMA
        | LCDC
        | DIV
        | Joypad

    let private IOAccessLookup address =
        sb {
            let! mb = SB.Get

            return
                match address with
                | address when 0xffffus = address -> IE
                | address when 0xff0fus = address -> IF
                | address when 0xff80us <= address && address <= 0xfffeus -> Array(mb.MMU.HRAM, (address - 0xff80us))
                | address when address = 0xFF47us -> BGF
                | address when address = 0xFF48us -> OBP0
                | address when address = 0xFF49us -> OBP1
                | address when address = 0xFF46us -> DMA
                | address when address = 0xFF45us -> LYC
                | address when address = 0xFF44us -> LY
                | address when address = 0xFF41us -> STAT
                | address when address = 0xFF42us -> SCY
                | address when address = 0xFF43us -> SCX
                | address when address = 0xFF40us -> LCDC
                | address when address = 0xFF04us -> DIV
                | address when address = 0xFF00us -> Joypad
                | address when 0xff00us <= address && address <= 0xff7fus -> Array(mb.MMU.IO, (address - 0xff00us))
                | address when 0xfea0us <= address && address <= 0xfeffus ->
                    Array(mb.MMU.UNUSABLE, (address - 0xfea0us))
                | address when 0xfe00us <= address && address <= 0xfe9fus -> Array(mb.MMU.OAM, (address - 0xfe00us))
                | address when 0xe000us <= address && address <= 0xfdffus -> Array(mb.MMU.WRAM, (address - 0xe000us))
                | address when 0xc000us <= address && address <= 0xdfffus -> Array(mb.MMU.WRAM, (address - 0xc000us))
                | address when 0xa000us <= address && address <= 0xbfffus -> Array(mb.MMU.EXRAM, (address - 0xa000us))
                | address when 0x8000us <= address && address <= 0x9fffus -> Array(mb.MMU.VRAM, (address - 0x8000us))
                | address when 0x0000us <= address && address <= 0x7fffus -> Array(mb.MMU.ROM, address)
                | _ -> raise (Exception("Failed to access " + address.ToString()))
        }

    let private ReadIOAccess access =
        sb {
            let! mb = SB.Get

            let value =
                match access with
                | Array (arr, adr) -> arr[int adr]
                | IE -> mb.CPU.IE
                | IF -> mb.CPU.IF
                | BGF -> mb.GPU.BGF
                | OBP0 -> mb.GPU.OBP0
                | OBP1 -> mb.GPU.OBP1
                | DMA -> mb.GPU.DMA
                | LYC -> mb.GPU.LYC
                | LY -> mb.GPU.LY
                | STAT -> mb.GPU.STAT
                | SCX -> mb.GPU.SCX
                | SCY -> mb.GPU.SCY
                | LCDC -> mb.GPU.LCDC
                | DIV -> byte (random.Next(255))
                | Joypad -> mb.Joypad

            return value
        }

    let ReadByte address =
        sb {
            let! lookup = IOAccessLookup address
            return! ReadIOAccess lookup
        }

    let private WriteIOAccess value access =
        sb {
            let! mb = SB.Get

            let mmb =
                match access with
                | Array (arr, adr) ->
                    arr[int adr] <- value
                    mb
                | IE -> { mb with CPU.IE = value }
                | IF -> assert(value < 2uy); { mb with CPU.IF = value }
                | BGF -> { mb with GPU.BGF = value }
                | OBP0 -> { mb with GPU.OBP0 = value }
                | OBP1 -> { mb with GPU.OBP1 = value }
                | DMA ->
                    { mb with
                        GPU =
                            { mb.GPU with
                                DMA = value
                                DMATransfer = true } }
                | LYC -> { mb with GPU.LYC = value }
                | LY -> { mb with GPU.LY = value }
                | STAT ->
                    { mb with
                        GPU =
                            { mb.GPU with
                                STAT =
                                    (mb.GPU.STAT &&& 0b111uy)
                                    &&& (value &&& 0b1111_1000uy) } }
                | SCX -> { mb with GPU.SCX = value }
                | SCY -> { mb with GPU.SCY = value }
                | LCDC -> { mb with GPU.LCDC = value }
                | Joypad -> { mb with Joypad = value }

            do! SB.Put mmb
        }


    let WriteByte address value =
        sb {
            if (address > 0x7FFFus) then
                // No ROM writing for now!
                let! lookup = IOAccessLookup address
                do! (WriteIOAccess value lookup)
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
