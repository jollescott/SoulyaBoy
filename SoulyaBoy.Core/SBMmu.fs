namespace SoulyaBoy.Core

open System

type SBMmu = {
    ROM: array<byte>;
    // TODO: Switchable banks.
    VRAM: array<byte>;
    WRAM: array<byte>;
    OAM: array<byte>;
    IO: array<byte>;
    HRAM: array<byte>;
    IE: byte;
}

module SBMmuFactory = 
    let internal CreateMMU(rom) = 
        { ROM = rom; VRAM = Array.zeroCreate 8000; WRAM = Array.zeroCreate 4000; 
          OAM = Array.zeroCreate 160;   IO = Array.zeroCreate 127;    HRAM = Array.zeroCreate 126; 
          IE = 0uy }

module internal MmuIO = 
    let private AddressLookup (mmu: SBMmu, address: uint16) = 
        match address with
        | address when 0x0000us < address && address < 0x3FFFus -> (mmu.ROM, address)
        | _ -> raise (new IndexOutOfRangeException($"{address} cannot be mapped to a location in memory."))

    let ReadByte mmu address =
        let (memory, local) = AddressLookup (mmu, address)
        memory[int(local)]

    let ReadShort mmu address = 
        let (memory, local) = AddressLookup (mmu, address)

        let low = memory[int(local)] 
        let high = memory[int(local) + 1]
        SBUtils.toShort (high, low)

    let WriteByte mmu address value = 
        let (memory, local) = AddressLookup (mmu, address)
        memory[int(local)] <- value
        

    let WriteShort mmu address value = 
        let (memory, local) = AddressLookup (mmu, address)
        let (high, low) = SBUtils.toBytes value

        memory[int(local)] <- low
        memory[int(local) + 1] <- high