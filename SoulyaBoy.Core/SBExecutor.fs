namespace SoulyaBoy.Core

open System

module internal SBExecutor =
    let internal N sb = 
        MmuIO.ReadByte sb.MMU (sb.CPU.PC + 1us)

    let internal NN sb = 
        MmuIO.ReadShort sb.MMU (sb.CPU.PC + 1us)

    let Execute sb = 
        let opcode = MmuIO.ReadByte sb.MMU sb.CPU.PC
        let instructions = SBOpcodes.INSTRUCTIONS

        if not <| instructions.ContainsKey(opcode) then 
            printf "Instruction %X is not implemented \n" opcode
            None
        else
            // Read the instruction
            let instruction = instructions[opcode] 

            // Resolve the operation, name & increment value for PC
            let (operation, _, pcd) = match instruction with 
                                            | NoExtra (Const(f), n) -> f, n, 1us
                                            | NoExtra (Void(f), n) -> f sb, n, 1us
                                            | NoExtra (Byte(NoArg(f)), n) -> f sb (N sb), n, 2us
                                            | NoExtra (Short(NoArg(f)), n) -> f sb (NN sb), n, 3us
                                            | Extra   (Byte(Arg(f)), n, x) -> f sb (N sb) x, n, 2us
                                            | Extra   (Short(Arg(f)), n, x) -> f sb (NN sb) x, n, 3us
                                            | _ -> raise(new InvalidOperationException("Illegal instruction type! (Tip, combine NoArg & Extra or Arg & NoExtra)"))

            // Increment CPU PC                                 
            let iCpu = { sb.CPU with PC = sb.CPU.PC + pcd}

            // Execute operation
            let (cycles, mCpu) = match operation with 
                                        | Result(Cycles(c)) -> c, iCpu
                                        | Result(Mutation(c, cpu)) -> c, cpu(iCpu)
                                        | Chain(func) -> func(iCpu)

            // Transfrom state & return
            let mutatedSb = { sb with CPU = mCpu }
            Some(cycles, mutatedSb)