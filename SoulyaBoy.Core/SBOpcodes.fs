namespace SoulyaBoy.Core

type internal SBMutation = SBMb -> option<SBMb>
type internal SBImmediateValue<'a> = SBMb -> option<'a> 
type internal SBInstructionProc<'a> = 'a -> int * option<SBMutation>
type internal SBInstructionRegister<'a> = SBInstructionProc<'a * byte> * (SBCpu -> byte)
type internal SBInstructionExtra<'a> = SBInstructionProc<'a * int> * int

type internal SBInstruction =
    | Const of SBInstructionProc<unit>
    | ConstExtra of SBInstructionExtra<unit>
    | Void of SBInstructionProc<unit>
    | VoidExtra of SBInstructionExtra<unit>
    | VoidRegister of SBInstructionRegister<unit>
    | Byte of SBInstructionProc<SBImmediateValue<byte>>
    | ByteExtra of SBInstructionExtra<SBImmediateValue<byte>>
    | ByteRegister of SBInstructionRegister<SBImmediateValue<byte>>
    | Short of SBInstructionProc<SBImmediateValue<uint16>>
    | ShortExtra of SBInstructionExtra<SBImmediateValue<uint16>>
    | ShortRegister of SBInstructionRegister<SBImmediateValue<uint16>>

type internal SBInstructionEntry = SBInstruction * string

type internal SBInstructionTable = Map<byte, SBInstructionEntry>

module SBOpcodes =

    let private sb = new SBBuilder()

    type internal Flags =
        | Z = 0b1000_0000uy
        | N = 0b0100_0000uy
        | H = 0b0010_0000uy
        | C = 0b0001_0000uy

    let internal Set flag = sb {
        let! mb = SB.Get
        do! SB.Put {mb with CPU = { mb.CPU with F = mb.CPU.F ||| (byte flag) }} 
    }   

    let internal Reset flag = sb {
        let! mb = SB.Get
        do! SB.Put { mb with CPU = { mb.CPU with F = mb.CPU.F &&& ~~~(byte flag) }}
    }

    let internal SetIf flag condition =
        if condition then Set flag else Reset flag

    let Execute (_, mutation) mb =
        match mutation with
        | Some x -> x mb
        | None -> mb

    module ByteLoads =

        let LD_A ni =
            let mut mb = 
                mb 
                |> ni
                |> SBUtils.bind (fun n -> Some { mb with CPU = { mb.CPU with A = n } })

            (8, Some(mut))

        let LD_B ni =
            let mut mb =
                mb
                |> ni
                |> SBUtils.bind (fun n -> Some { mb with CPU = { mb.CPU with B = n } })

            (8, Some(mut))

        let LD_C ni =
            let mut mb = 
                mb
                |> ni
                |> SBUtils.bind (fun n -> Some { mb with CPU = { mb.CPU with C = n } })

            (8, Some(mut))

        let LD_D ni =
            let mut mb = 
                mb
                |> ni
                |> SBUtils.bind (fun n -> Some { mb with CPU = { mb.CPU with D = n } })

            (8, Some(mut))

        let LD_H_HL () =
            let mut mb =
                let address = SBUtils.toShort mb.CPU.H mb.CPU.L

                mb 
                |> SBIO.ReadByte address
                |> SBUtils.bind (fun h -> Some { mb with CPU = { mb.CPU with H = h } })

            (8, Some(mut))

        let LD_HLD () =

            let updateHL h l mb = 
                Some { mb with CPU = { mb.CPU with H = h; L = l } }

            let mut mb =
                let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
                let h, l = SBUtils.toBytes (hl - 1us)
                
                mb 
                |> SBIO.WriteByte hl mb.CPU.A
                |> SBUtils.bind (updateHL h l)

            (8, Some(mut))

        let LD_n_A ni =
            let mut mb =
                mb 
                |> ni
                |> (fun n -> 0xFF00us + uint16 n)
                |> SBIO.WriteByte address mb.CPU.A

            (12, Some(mut))

        let LD_A_n n =
            let mut mb =
                let address = 0xFF00us + uint16 n
                let A = SBIO.ReadByte mb.MMU address
                { mb with CPU = { mb.CPU with A = A } }

            (12, Some(mut))

        let LD_HL_n n = 
            let mut mb = 
                let hl = SBUtils.toShort (mb.CPU.H, mb.CPU.L)
                SBIO.WriteByte mb.MMU hl n
                mb

            (12, Some(mut))

        let LD_nn_A nn = 
            let mut mb = 
                SBIO.WriteByte mb.MMU nn mb.CPU.A
                mb

            (16, Some(mut))

    module ShortLoads =
        let PUSH nn =
            let mut mb =
                let sp = mb.CPU.SP - 2us
                SBIO.WriteShort mb.MMU sp nn
                { mb with CPU = { mb.CPU with SP = sp } }

            (8, Some(mut))

    module Jump =
        let JP nn =
            let mut mb =
                { mb with CPU = { mb.CPU with PC = nn } }

            (16, Some(mut))

        let JR_NZ n =
            let mut mb =
                if mb.CPU.F &&& byte Flags.Z = 0uy then
                    let address = uint16 (int mb.CPU.PC + int (sbyte n))
                    Execute (JP address) mb
                else
                    mb

            (8, Some(mut))

        let RST ((), arg) =
            let mut mb =
                let address: uint16 = uint16 arg
                Execute (JP address) (Execute (ShortLoads.PUSH mb.CPU.PC) mb)

            (16, Some(mut))

    module Calls =
        let CALL nn =
            let mut mb =
                Execute (ShortLoads.PUSH mb.CPU.PC) mb |> Execute (Jump.JP nn)

            (12, Some(mut))

        let CALL_Z nn =
            let mut mb =
                if mb.CPU.F &&& byte Flags.Z <> 0uy then
                    Execute (CALL nn) mb
                else
                    mb

            (12, Some(mut))

    module ByteALU =
        let private DecFlags R r =
            SetIf Flags.Z (r = 0uy)
            >> Set Flags.N
            >> SetIf Flags.H ((R &&& 0b1000uy) <> (r &&& 0b1000uy))

        let DEC_B () =
            let mut mb =
                let b = mb.CPU.B - 1uy
                { mb with CPU = { mb.CPU with B = b } } |> DecFlags mb.CPU.B b

            (4, Some(mut))
            
        let DEC_C () =
            let mut mb =
                let c = mb.CPU.C - 1uy
                { mb with CPU = { mb.CPU with C = c } } |> DecFlags mb.CPU.C c
                
            (4, Some(mut))

        let DEC_D () =
            let mut mb =
                let d = mb.CPU.D - 1uy
                { mb with CPU = { mb.CPU with D = d } } |> DecFlags mb.CPU.D d

            (4, Some(mut))

        let DEC_E () =
            let mut mb =
                let e = mb.CPU.E - 1uy
                { mb with CPU = { mb.CPU with E = e } } |> DecFlags mb.CPU.E e

            (4, Some(mut))

        let ADC n =
            let mut mb =
                let carry: byte = (mb.CPU.F &&& byte Flags.C)
                let a = mb.CPU.A
                let r = a + n + carry
                { mb with CPU = { mb.CPU with A = r } }
                |> SetIf Flags.Z (r = 0uy)
                |> Reset Flags.N
                |> SetIf Flags.H ((a &&& 0b100uy) <> (r &&& 0b100uy))
                |> SetIf Flags.C ((a &&& 0b0100_0000uy) <> (r &&& 0b100_0000uy))

            (8, Some(mut))

        let XOR ((), rg) =
            let mut mb =
                let r: byte = rg ^^^ mb.CPU.A

                { mb with CPU = { mb.CPU with A = r } }
                |> SetIf Flags.Z (r = 0uy)
                |> Reset Flags.N
                |> Reset Flags.H
                |> Reset Flags.C

            (4, Some(mut))

        let CP_n n =
            let mut mb =
                let A = mb.CPU.A
                let r = A - n

                mb
                |> SetIf Flags.Z (r = 0uy)
                |> Set Flags.N
                // Unclear if borrow or "no" borrow.
                |> SetIf Flags.H ((A &&& 0b1000uy) = (r &&& 0b1000uy))
                |> SetIf Flags.C (A < n)

            (8, Some(mut))

    module ShortALU =
        let LD_HL nn =
            let mut mb =
                let (h: byte, l: byte) = SBUtils.toBytes nn
                { mb with CPU = { mb.CPU with H = h; L = l } }

            (12, Some(mut))

    module Control =
        let NOP () = (4, None)

    module Misc =
        let DI () =
            let mut mb =
                { mb with CPU = { mb.CPU with Interrupt = Disable } }

            (4, Some(mut))

    module RotatesShifts =
        let RRA () =
            let mut mb =
                let c: byte = mb.CPU.A &&& 0b1uy
                let a: byte = (mb.CPU.A >>> 1) ||| (c <<< 8)

                { mb with CPU = { mb.CPU with A = a } }
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
                             (0x66uy, (Void(ByteLoads.LD_H_HL), "LD H, (HL)"))
                             (0x32uy, (Void(ByteLoads.LD_HLD), "LD (HLD),A"))
                             (0xE0uy, (Byte(ByteLoads.LD_n_A), "LDH (n),A"))
                             (0xF0uy, (Byte(ByteLoads.LD_A_n), "LDH A,(n)"))
                             (0x36uy, (Byte(ByteLoads.LD_HL_n), "LD (HL), n"))
                             (0xEAuy, (Short(ByteLoads.LD_nn_A), "LD (nn),A"))

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
