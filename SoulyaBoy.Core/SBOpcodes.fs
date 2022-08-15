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
    | VoidRegister of (SB -> (SBCpu -> byte) -> SBInstructionResult)
    | Byte of (SB -> byte -> SBInstructionResult)
    | ByteExtra of (SB -> byte -> int -> SBInstructionResult) * int
    | ByteRegister of (SB -> byte -> (SBCpu -> byte) -> SBInstructionResult)
    | Short of (SB -> uint16 -> SBInstructionResult)
    | ShortExtra of (SB -> uint16 -> int -> SBInstructionResult) * int
    | ShortRegister of (SB -> uint16 -> (SBCpu -> byte) -> SBInstructionResult)

type internal SBInstruction = 
    SBInstructionType * string

type internal SBInstructionTable = Map<byte, SBInstruction>

module internal SBOpcodes = 
    let internal Delegate methods = 
        fun cpu -> match methods with
                    | Result(Cycles(_)),         Mutation(nc, mc) -> nc, mc(cpu)
                    | Result(Cycles(_)),         Cycles(nc) -> nc, cpu
                    | Result(Mutation(_, pm)),   Cycles(nc) -> nc, pm(cpu)
                    | Result(Mutation(_, pm)),   Mutation(nc, nm) -> nc, nm(pm(cpu))
                    | _ -> failwith "Chaining more than one level is not supported."

    module private ByteALU = 
        let ADC sb n = 
            let carry = (sb.CPU.F &&& 0b0001uy)
            Result(Mutation(8, fun cpu -> { cpu with A = sb.CPU.A + n + carry}))

    module private Control = 
        let NOP = Result(Cycles(4))

    module private Jump = 
        let JP _ nn = 
            Result(Mutation(16, fun cpu -> { cpu with PC = nn}))

        let RST sb nn arg = 
            let address = nn + uint16(arg)
            Chain(Delegate(JP sb address, Cycles(16)))
    
    let internal INSTRUCTIONS = SBInstructionTable [
        ( 0xCEuy, (Byte(ByteALU.ADC), "ADC A,n"))

        ( 0x0uy,  (Const(Control.NOP), "NOP"))

        ( 0xC3uy, (Short(Jump.JP), "JP NN"))
        ( 0xFFuy, (ShortExtra((Jump.RST), 0x38), "RST 38H"))
    ]

