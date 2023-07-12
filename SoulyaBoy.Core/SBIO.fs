namespace SoulyaBoy.Core


module SBIO =

    let sb = new SBBuilder()

    type private IOAccess = 
        | Array of array<byte> * uint16
        | IE
        | LY
        | LCDC

    let private IOAccessLookup address = sb {
         let! mb = SB.Get

         let access = match address with
                        | address when 0xffffus = address -> Some(IE)
                        | address when 0xff80us <= address && address <= 0xfffeus -> Some(Array(mb.MMU.HRAM, (address - 0xff80us)))
                        | address when address = 0xFF44us -> Some(LY)
                        | address when address = 0xFF40us -> Some(LCDC)
                        | address when 0xff00us <= address && address <= 0xff7fus -> Some(Array(mb.MMU.IO, (address - 0xff00us)))
                        | address when 0xfea0us <= address && address <= 0xfeffus -> Some(Array(mb.MMU.UNUSABLE, (address - 0xfea0us)))
                        | address when 0xfe00us <= address && address <= 0xfe9fus -> Some(Array(mb.MMU.OAM, (address - 0xfe00us)))
                        | address when 0xe000us <= address && address <= 0xfdffus -> Some(Array(mb.MMU.WRAM, (address - 0xe000us)))
                        | address when 0xc000us <= address && address <= 0xdfffus -> Some(Array(mb.MMU.WRAM, (address - 0xc000us)))
                        | address when 0xa000us <= address && address <= 0xbfffus -> Some(Array(mb.MMU.EXRAM, (address - 0xa000us)))
                        | address when 0x8000us <= address && address <= 0x9fffus -> Some(Array(mb.MMU.VRAM, (address - 0x8000us)))
                        | address when 0x0000us <= address && address <= 0x7fffus -> Some(Array(mb.MMU.ROM, address))
                        | _ -> None

         if access.IsNone then return! SB.Panic $"{address} does not match any memory locations" else return access.Value         
    }

    let private ReadIOAccess access = sb {
        let! mb = SB.Get

        return match access with
                | Array (arr ,adr) -> arr[int adr]
                | IE -> mb.CPU.IE
                | LY -> mb.GPU.LY 
                | LCDC -> mb.GPU.LCDC
    }     

    let ReadByte address = sb {
        let! lookup = IOAccessLookup address
        return! ReadIOAccess lookup
    }

    let private WriteIOAccess value access = sb {
        let! mb = SB.Get

        let mmb = match access with
                    | Array(arr, adr) -> arr[int adr] <- value; mb
                    | IE -> { mb with CPU = { mb.CPU with IE = value }}
                    | LY -> { mb with GPU = { mb.GPU with LY = value }}
                    | LCDC -> { mb with GPU = { mb.GPU with LCDC = value }}

        do! SB.Put mmb
    }


    let WriteByte address value = sb {
        let! lookup = IOAccessLookup address
        do! (WriteIOAccess value lookup)
    }


    let ReadShort address = sb {
        let! low = ReadByte address
        let! high = ReadByte (address + 1us)

        return SBUtils.toShort high low
    }

    let WriteShort address value = sb {
        let high, low = SBUtils.toBytes value
        do! (WriteByte address low)
        do! (WriteByte (address + 1us) high)
    }



