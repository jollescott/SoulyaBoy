namespace SoulyaBoy.Core


module SBIO =

    type private IOAccess = 
        | Array of array<byte> * uint16
        | IE of SB
        | LY of SB

    let private IOAccessLookup address sb =
        match address with
        | (address: uint16) when 0xffffus <= address -> Some(IE(sb))
        | (address: uint16) when 0xff80us <= address && address <= 0xfffeus -> Some(Array(sb.MMU.HRAM, (address - 0xff80us)))
        | (address: uint16) when 0xff00us <= address && address <= 0xff7fus -> Some(Array(sb.MMU.IO, (address - 0xff00us)))
        | (address: uint16) when 0xfea0us <= address && address <= 0xfeffus -> Some(Array(sb.MMU.UNUSABLE, (address - 0xfea0us)))
        | (address: uint16) when 0xfe00us <= address && address <= 0xfe9fus -> Some(Array(sb.MMU.OAM, (address - 0xfe00us)))
        | (address: uint16) when 0xe000us <= address && address <= 0xfdffus -> Some(Array(sb.MMU.WRAM, (address - 0xe000us)))
        | (address: uint16) when 0xc000us <= address && address <= 0xdfffus -> Some(Array(sb.MMU.WRAM, (address - 0xc000us)))
        | (address: uint16) when 0xa000us <= address && address <= 0xbfffus -> Some(Array(sb.MMU.EXRAM, (address - 0xa000us)))
        | (address: uint16) when 0x8000us <= address && address <= 0x9fffus -> Some(Array(sb.MMU.VRAM, (address - 0x8000us)))
        | (address: uint16) when 0x0000us <= address && address <= 0x7fffus -> Some(Array(sb.MMU.ROM, address))
        | _ -> None

    let private ReadIOAccess access =
        match access with
        | Array (arr ,adr) -> arr[int adr]
        | IE sb -> sb.CPU.IE
        | LY sb -> sb.GPU.LY        

    let ReadByte address =
        let read sb = 
            let lookup = IOAccessLookup address sb
            match lookup with
            | Some(access) -> Some (ReadIOAccess access)
            | None -> None
 
        read

    let private WriteIOAccess value access = 
        match access with
        | Array(arr,adr) -> arr[int adr] <- value; None
        | IE sb -> Some { sb with CPU = { sb.CPU with IE = value }}
        | LY sb -> Some { sb with GPU = { sb.GPU with LY = value}}


    let WriteByte address value = 
        let write sb = 
            IOAccessLookup address sb
            |> SBUtils.bind (WriteIOAccess value)

        write

    let private checkShortRead f (low: option<byte>) (high: option<byte>) =
        if low.IsNone || high.IsNone then
            None
        else 
            Some(f low.Value high.Value)

    let ReadShort address =
        let readLow = ReadByte address
        let readHigh = ReadByte (address + 1us)

        let read sb =
            (readLow sb, readHigh sb)
            ||> checkShortRead SBUtils.toShort

        read

    let WriteShort address value =
        let high, low = SBUtils.toBytes value

        let writeLow = WriteByte address low
        let writeHigh = WriteByte (address + 1us) high

        let write sb = 
            writeLow sb
            |> SBUtils.bind writeHigh

        write



