namespace SoulyaBoy.Core

type internal SBMutation = SB -> SB

type internal SBInstructionProc<'a> = 'a -> int * option<SBMutation>

type internal SBInstructionRegister<'a> = SBInstructionProc<'a * byte> * (SBCpu -> byte)

type internal SBInstructionExtra<'a> = SBInstructionProc<'a * int> * int

type internal SBInstruction =
    | Const of SBInstructionProc<unit>
    | ConstExtra of SBInstructionExtra<unit>
    | Void of SBInstructionProc<unit>
    | VoidExtra of SBInstructionExtra<unit>
    | VoidRegister of SBInstructionRegister<unit>
    | Byte of SBInstructionProc<byte>
    | ByteExtra of SBInstructionExtra<byte>
    | ByteRegister of SBInstructionRegister<byte>
    | Short of SBInstructionProc<uint16>
    | ShortExtra of SBInstructionExtra<uint16>
    | ShortRegister of SBInstructionRegister<uint16>

type internal SBInstructionEntry = SBInstruction * string

type internal SBInstructionTable = Map<byte, SBInstructionEntry>

module SBOpcodes =
    let Execute (_, mutation) sb =
        match mutation with
        | Some (x) -> x sb
        | None -> sb

    module Jump =
        let JP nn =
            let mut sb =
                { sb with CPU = { sb.CPU with PC = nn } }

            (16, Some(mut))

        let JR_NZ n =
            let mut sb =
                if sb.CPU.F &&& 0b1000_0000uy = 0uy then
                    let address = uint16 (int (sb.CPU.PC) + int (sbyte (n)))
                    Execute (JP address) sb
                else
                    sb

            (8, Some(mut))

        let RST (nn, arg) =
            let mut sb =
                let address: uint16 = nn + uint16 (arg)
                Execute (JP address) sb

            (16, Some(mut))

    module ByteLoads =

        let LD_A n =
            let mut sb = { sb with CPU = { sb.CPU with A = n } }

            (8, Some(mut))

        let LD_B n =
            let mut sb = { sb with CPU = { sb.CPU with B = n } }

            (8, Some(mut))

        let LD_C n =
            let mut sb = { sb with CPU = { sb.CPU with C = n } }

            (8, Some(mut))

        let LD_D n =
            let mut sb = { sb with CPU = { sb.CPU with D = n } }

            (8, Some(mut))

        let LD_H_HL () =
            let mut sb =
                let address = SBUtils.toShort (sb.CPU.H, sb.CPU.L)
                let h = MmuIO.ReadByte sb.MMU address
                { sb with CPU = { sb.CPU with H = h } }

            (8, Some(mut))

        let LD_HLD () =
            let mut sb =
                let hl: uint16 = SBUtils.toShort (sb.CPU.H, sb.CPU.L) - 1us
                let (h: byte, l: byte) = SBUtils.toBytes (hl)

                MmuIO.WriteByte sb.MMU hl sb.CPU.A
                { sb with CPU = { sb.CPU with H = h; L = l } }

            (8, Some(mut))

        let LD_n_A n =
            let mut sb =
                let address = 0xFF00us + uint16 (n)
                MmuIO.WriteByte sb.MMU address sb.CPU.A
                sb

            (12, Some(mut))

        let LD_A_n n =
            let mut sb =
                let address = 0xFF00us + uint16 (n)
                let A = MmuIO.ReadByte sb.MMU address
                { sb with CPU = { sb.CPU with A = A } }

            (12, Some(mut))

    module ShortLoads =
        let PUSH nn =
            let mut sb =
                MmuIO.WriteShort sb.MMU sb.CPU.SP nn
                let sp = sb.CPU.SP - 2us
                { sb with CPU = { sb.CPU with SP = sp } }

            (8, Some(mut))

    module Calls =

        let CALL nn =
            let mut sb =
                let pushSb = Execute (ShortLoads.PUSH sb.CPU.PC) sb
                Execute (Jump.JP nn) pushSb

            (12, Some(mut))

        let CALL_Z sb nn =
            let mut sb =
                if sb.CPU.F &&& 0b1000_0000uy <> 0uy then
                    Execute (CALL nn) sb
                else
                    sb

            (12, Some(mut))

    module ByteALU =
        // TODO: Cleanup
        let private DEC_Flags o r f =
            if r = 0uy then
                f ||| 0b1000_0000uy
            else
                f &&& ~~~ 0b1000_0000uy
            |> fun z ->
                z
                ||| (if (o &&& 0b1000uy) = (r &&& 0b1000uy) then
                         f ||| 0b0010_0000uy
                     else
                         f &&& ~~~ 0b0010_0000uy)
            |> fun h -> h ||| 0b0100_0000uy

        let DEC_B () =
            let mut sb =
                let b = sb.CPU.B - 1uy
                let f = DEC_Flags sb.CPU.B b sb.CPU.F
                { sb with CPU = { sb.CPU with B = b; F = f } }

            (4, Some(mut))

        let DEC_D () =
            let mut sb =
                let d = sb.CPU.D - 1uy
                let f = DEC_Flags sb.CPU.D d sb.CPU.F
                { sb with CPU = { sb.CPU with D = d; F = f } }

            (4, Some(mut))

        let DEC_E () =
            let mut sb =
                let e = sb.CPU.E - 1uy
                let f = DEC_Flags sb.CPU.E e sb.CPU.F
                { sb with CPU = { sb.CPU with E = e; F = f } }

            (4, Some(mut))

        let ADC n =
            let mut sb =
                let carry: byte = (sb.CPU.F &&& 0b0001_0000uy)
                { sb with CPU = { sb.CPU with A = sb.CPU.A + n + carry } }

            (8, Some(mut))

        let XOR ((), r) =
            let mut sb =
                let result: byte = sb.CPU.A ^^^ r
                let flags: byte = if result = 0uy then 0uy else 0b1000uy
                { sb with CPU = { sb.CPU with A = result; F = flags } }

            (4, Some(mut))

        let CP_n n =
            let mut sb =
                let r = sb.CPU.A - n

                let f =
                    sb.CPU.F
                    |> fun f -> f ||| (if r = 0uy then 0b1000_0000uy else 0uy)
                    |> fun z -> z ||| 0b0100_0000uy
                    |> fun n ->
                        n
                        ||| (if (sb.CPU.A &&& 0b1000uy) <> (r &&& 0b1000uy) then
                                 0uy
                             else
                                 0b0010_0000uy)
                    |> fun h -> h ||| (if sb.CPU.A < n then 0b0001_0000uy else 0uy)

                { sb with CPU = { sb.CPU with F = f } }

            (8, Some(mut))

    module ShortALU =
        let LD_HL nn =
            let mut sb =
                let (h: byte, l: byte) = SBUtils.toBytes nn
                { sb with CPU = { sb.CPU with H = h; L = l } }

            (12, Some(mut))

    module Control =
        let NOP () = (4, None)

    module Misc =
        let DI () =
            let mut sb =
                { sb with CPU = { sb.CPU with Interrupt = Disable } }

            (4, Some(mut))

    module RotatesShifts =
        let RRA () =
            let mut sb =
                let c: byte = sb.CPU.A &&& 0b1uy
                let a: byte = (sb.CPU.A >>> 1) &&& (c <<< 8)

                let flags: byte = (if a = 0uy then 0b1000_0000uy else 0b0uy) &&& c <<< 8
                { sb with CPU = { sb.CPU with A = a; F = flags } }

            (4, Some(mut))

    let internal INSTRUCTIONS =
        SBInstructionTable [ (0x3Euy, (Byte(ByteLoads.LD_A), "LD A,n"))
                             (0x06uy, (Byte(ByteLoads.LD_B), "LD B,n"))
                             (0x0Euy, (Byte(ByteLoads.LD_C), "LD C,n"))
                             (0x16uy, (Byte(ByteLoads.LD_D), "LD D,n"))
                             (0x66uy, (Void(ByteLoads.LD_H_HL), "LD, (HL)"))
                             (0x32uy, (Void(ByteLoads.LD_HLD), "LD (HLD),A"))
                             (0xE0uy, (Byte(ByteLoads.LD_n_A), "LDH (n),A"))
                             (0xF0uy, (Byte(ByteLoads.LD_A_n), "LDH A,(n)"))

                             (0x05uy, (Void(ByteALU.DEC_B), "DEC B"))
                             (0x0Duy, (Void(ByteALU.DEC_D), "DEC D"))
                             (0x1Duy, (Void(ByteALU.DEC_E), "DEC E"))
                             (0xCEuy, (Byte(ByteALU.ADC), "ADC A,n"))
                             (0xAFuy, (VoidRegister(ByteALU.XOR, (fun cpu -> cpu.A)), "XOR A"))
                             (0xFEuy, (Byte(ByteALU.CP_n), "CP #"))

                             (0x0uy, (Const(Control.NOP), "NOP"))

                             (0xC3uy, (Short(Jump.JP), "JP NN"))
                             (0x20uy, (Byte(Jump.JR_NZ), "JR NZ"))
                             (0xFFuy, (ShortExtra(Jump.RST, 0x38), "RST 38H"))

                             (0x21uy, (Short(ShortALU.LD_HL), "LD_HL"))

                             (0xF3uy, (Void(Misc.DI), "Misc"))

                             (0x1Fuy, (Void(RotatesShifts.RRA), "RRA")) ]
