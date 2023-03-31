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
    type internal Flags =
        | Z = 0b1000_0000uy
        | N = 0b0100_0000uy
        | H = 0b0010_0000uy
        | C = 0b0001_0000uy

    let internal Set flag =
        fun sb -> { sb with CPU = { sb.CPU with F = sb.CPU.F ||| byte flag } }

    let internal Reset flag =
        fun sb -> { sb with CPU = { sb.CPU with F = sb.CPU.F &&& ~~~(byte flag) } }

    let internal SetIf flag condition =
        if condition then Set flag else Reset flag

    let Execute (_, mutation) sb =
        match mutation with
        | Some x -> x sb
        | None -> sb

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
                let hl = SBUtils.toShort (sb.CPU.H, sb.CPU.L)
                MmuIO.WriteByte sb.MMU hl sb.CPU.A

                // TODO: Move to DEC_HL
                let (h: byte, l: byte) = SBUtils.toBytes (hl - 1us)
                { sb with CPU = { sb.CPU with H = h; L = l } }

            (8, Some(mut))

        let LD_n_A n =
            let mut sb =
                let address = 0xFF00us + uint16 n
                MmuIO.WriteByte sb.MMU address sb.CPU.A
                sb

            (12, Some(mut))

        let LD_A_n n =
            let mut sb =
                let address = 0xFF00us + uint16 n
                let A = MmuIO.ReadByte sb.MMU address
                { sb with CPU = { sb.CPU with A = A } }

            (12, Some(mut))

    module ShortLoads =
        let PUSH nn =
            let mut sb =
                let sp = sb.CPU.SP - 2us
                MmuIO.WriteShort sb.MMU sp nn
                { sb with CPU = { sb.CPU with SP = sp } }

            (8, Some(mut))

    module Jump =
        let JP nn =
            let mut sb =
                { sb with CPU = { sb.CPU with PC = nn } }

            (16, Some(mut))

        let JR_NZ n =
            let mut sb =
                if sb.CPU.F &&& byte Flags.Z = 0uy then
                    let address = uint16 (int sb.CPU.PC + int (sbyte n))
                    Execute (JP address) sb
                else
                    sb

            (8, Some(mut))

        let RST ((), arg) =
            let mut sb =
                let address: uint16 = uint16 arg
                Execute (JP address) (Execute (ShortLoads.PUSH sb.CPU.PC) sb)

            (16, Some(mut))

    module Calls =
        let CALL nn =
            let mut sb =
                Execute (ShortLoads.PUSH sb.CPU.PC) sb |> Execute (Jump.JP nn)

            (12, Some(mut))

        let CALL_Z nn =
            let mut sb =
                if sb.CPU.F &&& byte Flags.Z <> 0uy then
                    Execute (CALL nn) sb
                else
                    sb

            (12, Some(mut))

    module ByteALU =
        let private DecFlags R r =
            SetIf Flags.Z (r = 0uy)
            >> Set Flags.N
            >> SetIf Flags.H ((R &&& 0b1000uy) <> (r &&& 0b1000uy))

        let DEC_B () =
            let mut sb =
                let b = sb.CPU.B - 1uy
                { sb with CPU = { sb.CPU with B = b } } |> DecFlags sb.CPU.B b

            (4, Some(mut))
            
        let DEC_C () =
            let mut sb =
                let c = sb.CPU.C - 1uy
                { sb with CPU = { sb.CPU with C = c } } |> DecFlags sb.CPU.C c
                
            (4, Some(mut))

        let DEC_D () =
            let mut sb =
                let d = sb.CPU.D - 1uy
                { sb with CPU = { sb.CPU with D = d } } |> DecFlags sb.CPU.D d

            (4, Some(mut))

        let DEC_E () =
            let mut sb =
                let e = sb.CPU.E - 1uy
                { sb with CPU = { sb.CPU with E = e } } |> DecFlags sb.CPU.E e

            (4, Some(mut))

        let ADC n =
            let mut sb =
                let carry: byte = (sb.CPU.F &&& byte Flags.C)
                let a = sb.CPU.A
                let r = a + n + carry
                { sb with CPU = { sb.CPU with A = r } }
                |> SetIf Flags.Z (r = 0uy)
                |> Reset Flags.N
                |> SetIf Flags.H ((a &&& 0b100uy) <> (r &&& 0b100uy))
                |> SetIf Flags.C ((a &&& 0b0100_0000uy) <> (r &&& 0b100_0000uy))

            (8, Some(mut))

        let XOR ((), rg) =
            let mut sb =
                let r: byte = rg ^^^ sb.CPU.A

                { sb with CPU = { sb.CPU with A = r } }
                |> SetIf Flags.Z (r = 0uy)
                |> Reset Flags.N
                |> Reset Flags.H
                |> Reset Flags.C

            (4, Some(mut))

        let CP_n n =
            let mut sb =
                let A = sb.CPU.A
                let r = A - n

                sb
                |> SetIf Flags.Z (r = 0uy)
                |> Set Flags.N
                // Unclear if borrow or "no" borrow.
                |> SetIf Flags.H ((A &&& 0b1000uy) = (r &&& 0b1000uy))
                |> SetIf Flags.C (A < n)

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
                let a: byte = (sb.CPU.A >>> 1) ||| (c <<< 8)

                { sb with CPU = { sb.CPU with A = a } }
                |> SetIf Flags.Z (a = 0uy)
                |> Reset Flags.N
                |> Reset Flags.H
                |> SetIf Flags.C (c = 1uy)

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
                             (0x0Duy, (Void(ByteALU.DEC_C), "DEC C"))
                             (0x1Duy, (Void(ByteALU.DEC_E), "DEC E"))
                             (0xCEuy, (Byte(ByteALU.ADC), "ADC A,n"))
                             (0xAFuy, (VoidRegister(ByteALU.XOR, (fun cpu -> cpu.A)), "XOR A"))
                             (0xFEuy, (Byte(ByteALU.CP_n), "CP #"))

                             (0x0uy, (Const(Control.NOP), "NOP"))

                             (0xC3uy, (Short(Jump.JP), "JP NN"))
                             (0x20uy, (Byte(Jump.JR_NZ), "JR NZ"))
                             (0xFFuy, (VoidExtra(Jump.RST, 0x38), "RST 38H"))

                             (0xCDuy, (Short(Calls.CALL), "CALL nn"))
                             (0xCCuy, (Short(Calls.CALL_Z), "CALL Z,nn"))

                             (0x21uy, (Short(ShortALU.LD_HL), "LD_HL"))

                             (0xF3uy, (Void(Misc.DI), "Misc"))

                             (0x1Fuy, (Void(RotatesShifts.RRA), "RRA")) ]
