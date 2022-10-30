namespace SoulyaBoy.Core

open System

module internal SBExecutor =
    let internal N (sb: SB) = MmuIO.ReadByte sb.MMU (sb.CPU.PC + 1us)

    let internal NN (sb: SB) =
        MmuIO.ReadShort sb.MMU (sb.CPU.PC + 1us)

    let Execute sb =
        let opcode = MmuIO.ReadByte sb.MMU sb.CPU.PC
        let instructions: SBInstructionTable = SBOpcodes.INSTRUCTIONS

        if not <| instructions.ContainsKey(opcode) then
            printf "Instruction %X is not implemented \n" opcode
            None
        else
            // Read the instruction
            let instruction = instructions[opcode]

            // Resolve the operation, name & increment value for PC
            let (operation, _, pcd) =
                match instruction with
                | (Const (c), n) -> c, n, 1us
                | (ConstExtra (c, x), n) -> c x, n, 1us
                | (Void (f), n) -> f sb, n, 1us
                | (VoidExtra (f, x), n) -> f sb x, n, 1us
                | (VoidRegister (f, x), n) -> f sb (x sb.CPU), n, 1us
                | (Byte (f), n) -> f sb (N sb), n, 2us
                | (ByteExtra (f, x), n) -> f sb (N sb) x, n, 2us
                | (Short (f), n) -> f sb (NN sb), n, 3us
                | (ShortExtra (f, x), n) -> f sb (NN sb) x, n, 3us
                | _ -> raise (new InvalidOperationException("Illegal instruction type!"))

            // Increment CPU PC
            let iCpu = { sb.CPU with PC = sb.CPU.PC + pcd }

            // Execute operation
            let (cycles, mCpu) =
                match operation with
                | Result (Cycles (c)) -> c, iCpu
                | Result (Mutation (c, cpu)) -> c, cpu (iCpu)
                | Chain (func) -> func (iCpu)

            // Transfrom state & return
            let mutatedSb = { sb with CPU = mCpu }
            Some(cycles, mutatedSb)
