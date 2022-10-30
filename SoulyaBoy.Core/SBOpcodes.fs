namespace SoulyaBoy.Core

type internal SBInstructionReturn =
    | Cycles of int
    | Mutation of int * (SBCpu -> SBCpu)

type internal SBInstructionResult =
    | Result of SBInstructionReturn
    | Chain of (SBCpu -> int * SBCpu)

type internal SBInstructionType =
    | Const of SBInstructionResult
    | ConstExtra of (int -> SBInstructionResult) * int
    | Void of (SB -> SBInstructionResult)
    | VoidExtra of (SB -> int -> SBInstructionResult) * int
    | VoidRegister of (SB -> byte -> SBInstructionResult) * (SBCpu -> byte)
    | Byte of (SB -> byte -> SBInstructionResult)
    | ByteExtra of (SB -> byte -> int -> SBInstructionResult) * int
    | ByteRegister of (SB -> byte -> (SBCpu -> byte) -> SBInstructionResult)
    | Short of (SB -> uint16 -> SBInstructionResult)
    | ShortExtra of (SB -> uint16 -> int -> SBInstructionResult) * int
    | ShortRegister of (SB -> uint16 -> (SBCpu -> byte) -> SBInstructionResult)

type internal SBInstruction = SBInstructionType * string

type internal SBInstructionTable = Map<byte, SBInstruction>

module internal SBOpcodes =
    let internal Delegate (methods: SBInstructionResult * SBInstructionReturn) =
        fun (cpu: SBCpu) ->
            match methods with
            | Result (Cycles (_)), Mutation (nc: int, mc) -> nc, mc (cpu)
            | Result (Cycles (_)), Cycles (nc: int) -> nc, cpu
            | Result (Mutation (_, pm)), Cycles (nc: int) -> nc, pm (cpu)
            | Result (Mutation (_, pm)), Mutation (nc: int, nm) -> nc, nm (pm (cpu))
            | _ -> failwith "Chaining more than one level is not supported."

    module private Jump =
        let JP _ (nn: uint16) =
            Result(Mutation(16, (fun (cpu: SBCpu) -> { cpu with PC = nn })))

        let JR_NZ (sb: SB) (n: byte) =
            if sb.CPU.F &&& 0b1000_0000uy = 0uy then
                Chain(Delegate(JP sb (sb.CPU.PC + uint16 (n)), Cycles(8)))
            else
                Result(Cycles(8))

        let RST (sb: 'a) (nn: uint16) (arg: int) =
            let address: uint16 = nn + uint16 (arg)
            Chain(Delegate(JP sb address, Cycles(16)))

    module private ByteLoads =
        let LD_B _ (n: byte) =
            Result(Mutation(8, (fun (cpu: SBCpu) -> { cpu with B = n })))

        let LD_C _ (n: byte) =
            Result(Mutation(8, (fun (cpu: SBCpu) -> { cpu with C = n })))

        let LD_D _ (n: byte) =
            Result(Mutation(8, (fun (cpu: SBCpu) -> { cpu with D = n })))

        let LD_HLD (sb: SB) =
            let hl: uint16 = SBUtils.toShort (sb.CPU.H, sb.CPU.L) - 1us
            let (h: byte, l: byte) = SBUtils.toBytes (hl)

            MmuIO.WriteByte sb.MMU hl sb.CPU.A

            Result(Mutation(8, (fun (cpu: SBCpu) -> { cpu with H = h; L = l })))

    module private ByteALU =
        let private DEC_Flags o r f =
            f
            &&& (if r <> 0uy then 0b1000_0000uy else 0uy)
            &&& 0b0100_0000uy
            &&& (if (o &&& 0b1000uy) = (r &&& 0b1000uy) then
                     0b0001_0000uy
                 else
                     0uy)

        let DEC_B (sb: SB) =
            let b = sb.CPU.B - 1uy
            let f = DEC_Flags sb.CPU.B b sb.CPU.F
            Result(Mutation(4, (fun (cpu: SBCpu) -> { cpu with B = b; F = f })))

        let DEC_E (sb: SB) =
            let e = sb.CPU.E - 1uy
            let f = DEC_Flags sb.CPU.E e sb.CPU.F
            Result(Mutation(4, (fun (cpu: SBCpu) -> { cpu with E = e; F = f })))

        let ADC (sb: SB) (n: byte) =
            let carry: byte = (sb.CPU.F &&& 0b0001_0000uy)
            Result(Mutation(8, (fun (cpu: SBCpu) -> { cpu with A = sb.CPU.A + n + carry })))

        let XOR (sb: SB) (x: byte) =
            let result: byte = sb.CPU.A ^^^ x
            let flags: byte = if result = 0uy then 0uy else 0b1000uy

            Result(Mutation(4, (fun (cpu: SBCpu) -> { cpu with A = result; F = flags })))

    module private ShortALU =
        let LD_HL _ (nn: uint16) =
            let (h: byte, l: byte) = SBUtils.toBytes nn
            Result(Mutation(12, (fun (cpu: SBCpu) -> { cpu with H = h; L = l })))



    module private Control =
        let NOP = Result(Cycles(4))

    let internal INSTRUCTIONS =
        SBInstructionTable [ (0x06uy, (Byte(ByteLoads.LD_B), "LD B,n"))
                             (0x0Euy, (Byte(ByteLoads.LD_C), "LD C,n"))
                             (0x16uy, (Byte(ByteLoads.LD_D), "LD D,n"))
                             (0x32uy, (Void(ByteLoads.LD_HLD), "LD (HLD),A"))

                             (0x05uy, (Void(ByteALU.DEC_B), "DEC B"))
                             (0x1Duy, (Void(ByteALU.DEC_E), "DEC E"))
                             (0xCEuy, (Byte(ByteALU.ADC), "ADC A,n"))
                             (0xAFuy, (VoidRegister(ByteALU.XOR, (fun (cpu: SBCpu) -> cpu.A)), "XOR A"))

                             (0x0uy, (Const(Control.NOP), "NOP"))

                             (0xC3uy, (Short(Jump.JP), "JP NN"))
                             (0x20uy, (Byte(Jump.JR_NZ), "JR NZ"))
                             (0xFFuy, (ShortExtra((Jump.RST), 0x38), "RST 38H"))
                             (0x21uy, (Short(ShortALU.LD_HL), "LD_HL")) ]
