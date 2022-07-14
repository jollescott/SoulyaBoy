namespace SoulyaBoy.Core

type internal SBInstructionReturn = 
    | Cycles of int 
    | Mutation of int * (SBCpu -> SBCpu)

type internal SBInstructionResult = 
    | Result of SBInstructionReturn
    | Chain of (SBCpu -> int * SBCpu)

type internal SBArgumentInstruction<'a> = 
    | NoArg of (SB -> 'a -> SBInstructionResult)
    | Arg of (SB -> 'a -> byte -> SBInstructionResult)

type internal SBInstructionRoutine = 
    | Const of SBInstructionResult
    | Void of (SB -> SBInstructionResult)
    | Byte of SBArgumentInstruction<byte>
    | Short of SBArgumentInstruction<uint16>
 
type internal SBInstruction = 
    | NoExtra of SBInstructionRoutine * string
    | Extra of SBInstructionRoutine * string * byte

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

        let RST sb n arg = 
            let address = uint16(n + arg)
            Chain(Delegate(JP sb address, Cycles(16)))
    
    let internal INSTRUCTIONS = Map [
        ( 0xCEuy, NoExtra(Byte(NoArg(ByteALU.ADC)), "ADC A,n"))

        ( 0x0uy,  NoExtra(Const(Control.NOP), "NOP"))

        ( 0xC3uy, NoExtra(Short(NoArg(Jump.JP)), "JP NN"))
        ( 0xFFuy, Extra(Byte(Arg(Jump.RST)), "RST 38H", 0x38uy))
    ]

