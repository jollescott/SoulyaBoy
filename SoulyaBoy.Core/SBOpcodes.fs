namespace SoulyaBoy.Core

type SBOpcodeRoutine = 
    | NoImmediate of int
    | Immediate of (byte -> int)
    | DoubleImmediate of (int16 -> int) 
   
type SBOpcode = {
    Routine: SBOpcodeRoutine
}

module SBOpcodes = 
    let NOP = 10
    
    let internal OPCODES = Map [
        ( 0x0us, { Routine = NoImmediate(NOP) })
    ]

    let internal Immediate sb = 
        MmuIO.ReadByte sb.MMU (int(sb.CPU.PC + 1us))

    let internal DoubleImmediate sb = 
        MmuIO.ReadShort sb.MMU (int(sb.CPU.PC + 1us))

    let Execute sb = 
        let opcode = OPCODES[sb.CPU.PC]
        let routine = opcode.Routine

        let (cycles, pcd) = match routine with
            | SBOpcodeRoutine.NoImmediate x -> x, 1us
            | SBOpcodeRoutine.Immediate f -> f (Immediate sb), 2us
            | SBOpcodeRoutine.DoubleImmediate f -> f (DoubleImmediate sb), 3us

        ()