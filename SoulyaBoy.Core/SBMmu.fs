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
    let private AddressLookup mmu address = 
        match address with
        | address when 0x0000 < address && address < 0x3FFF -> Some (mmu.ROM, address)
        | _ -> None

    let ReadByte mmu address =
        let result = AddressLookup mmu address

        match result with
        | Some (memory, local) -> Some memory[local]
        | None -> None

    let ReadShort mmu address = 
        ()

    let WriteByte mmu address value = 
        ()

    let WriteShort mmu address value = 
        ()