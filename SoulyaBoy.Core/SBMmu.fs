namespace SoulyaBoy.Core

open System

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
          IO = Array.zeroCreate 127
          HRAM = Array.zeroCreate 126
          IE = Array.zeroCreate 1 }

module internal MmuIO =
    let private AddressLookup (mmu: SBMmu, address: uint16) =
        match address with
        | (address: uint16) when 0xffffus <= address -> (mmu.IE, address - 0xffffus)
        | (address: uint16) when 0xff00us <= address && address <= 0xff7fus -> (mmu.IO, address - 0xff00us)
        | (address: uint16) when 0xfea0us <= address && address <= 0xfeffus -> (mmu.UNUSABLE, address - 0xfea0us)
        | (address: uint16) when 0xfe00us <= address && address <= 0xfe9fus -> (mmu.OAM, address - 0xfe00us)
        | (address: uint16) when 0xe000us <= address && address <= 0xfdffus -> (mmu.WRAM, address - 0xe000us)
        | (address: uint16) when 0xc000us <= address && address <= 0xdfffus -> (mmu.WRAM, address - 0xc000us)
        | (address: uint16) when 0xa000us <= address && address <= 0xbfffus -> (mmu.EXRAM, address - 0xa000us)
        | (address: uint16) when 0x8000us <= address && address <= 0x9fffus -> (mmu.VRAM, address - 0x8000us)
        | (address: uint16) when 0x0000us <= address && address <= 0x7fffus -> (mmu.ROM, address)
        | _ -> raise (new IndexOutOfRangeException($"{address} cannot be mapped to a location in memory."))

    let ReadByte (mmu: SBMmu) (address: uint16) =
        let (memory: byte array, local: uint16) = AddressLookup(mmu, address)
        memory[int (local)]

    let ReadShort mmu address =
        let (memory, local) = AddressLookup(mmu, address)

        let low = memory[int (local)]
        let high = memory[int (local) + 1]
        SBUtils.toShort (high, low)

    let WriteByte mmu address value =
        let (memory, local) = AddressLookup(mmu, address)
        memory[int (local)] <- value


    let WriteShort mmu address value =
        let (memory, local) = AddressLookup(mmu, address)
        let (high, low) = SBUtils.toBytes value

        memory[int (local)] <- low
        memory[int (local) + 1] <- high
