namespace SoulyaBoy.Core

open System

module internal SBExecutor =
    let internal N (sb: SB) = MmuIO.ReadByte sb.MMU (sb.CPU.PC + 1us)

    let internal NN (sb: SB) =
        MmuIO.ReadShort sb.MMU (sb.CPU.PC + 1us)

    type private OperationFunc =
        | Unit of (unit -> SBInstructionResult)
        | SB of (SB -> SBInstructionResult)

    let Execute bsb _ =
        let opcode = MmuIO.ReadByte bsb.MMU bsb.CPU.PC
        let instructions: SBInstructionTable = SBOpcodes.INSTRUCTIONS

        if not <| instructions.ContainsKey(opcode) then
            printf "Instruction %X is not implemented \n" opcode
            None
        else
            // Read the instruction
            let instruction = instructions[opcode]

            let ni = N bsb
            let nni = NN bsb

            // Resolve the operation, name & increment value for PC
            let (operationFunc, _, pcd) =
                match instruction with
                | (Const (c), n) -> Unit(fun () -> c), n, 0us
                | (ConstExtra (c, x), n) -> Unit(fun () -> c x), n, 0us
                | (Void (f), n) -> SB(fun (sb) -> f sb), n, 0us
                | (VoidExtra (f, x), n) -> SB(fun sb -> f sb x), n, 0us
                | (VoidRegister (f, x), n) -> SB(fun sb -> f sb (x sb.CPU)), n, 0us
                | (Byte (f), n) -> SB(fun sb -> f sb ni), n, 1us
                | (ByteExtra (f, x), n) -> SB(fun sb -> f sb ni x), n, 1us
                | (Short (f), n) -> SB(fun sb -> f sb nni), n, 2us
                | (ShortExtra (f, x), n) -> SB(fun sb -> f sb nni x), n, 2us
                | _ -> raise (new InvalidOperationException("Illegal instruction type!"))

            // Pre instruction call PC increment
            let isb = { bsb with CPU = {bsb.CPU with PC = bsb.CPU.PC + 1us + pcd}}

            let operation = 
                match operationFunc with
                | Unit(f) -> f()
                | SB(f) -> f(isb) 

            let icpu = isb.CPU

            // Execute operation
            let (cycles, mCpu) =
                match operation with
                | Result (Cycles (c)) -> c, icpu
                | Result (Mutation (c, cpu)) -> c, cpu icpu
                | Chain (func) -> func icpu

            let fcpu = match bsb.CPU.Interupt with
                | Disable -> { icpu with Interupt = Disabled }
                | Enable -> { icpu with Interupt = Enabled }
                | _ -> icpu

            // Transfrom state & return
            let fsb = { isb with CPU = fcpu }
            Some(cycles, fsb)
