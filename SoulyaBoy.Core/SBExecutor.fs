namespace SoulyaBoy.Core


module internal SBExecutor =
    let internal N (sb: SB) = MmuIO.ReadByte sb.MMU (sb.CPU.PC + 1us)
    let internal NN (sb: SB) = MmuIO.ReadShort sb.MMU (sb.CPU.PC + 1us)

    let Execute bsb _ =
        let opcode = MmuIO.ReadByte bsb.MMU bsb.CPU.PC
        let instructions: SBInstructionTable = SBOpcodes.INSTRUCTIONS
                
        if not <| instructions.ContainsKey(opcode) then
            printf $"Instruction %X{opcode} is not implemented \n"
            None
        else
            // Read the instruction
            let instruction = instructions[opcode]
                        
            let ni = N bsb
            let nni = NN bsb

            // Resolve the operation, name & increment value for PC
            let operationFunc, _, pcd =
                match instruction with
                | Const c, n -> c (), n, 0us
                | ConstExtra (c, e), n -> c ((), e), n, 0us
                | Void f, n -> f (), n, 0us
                | VoidExtra (f, e), n -> f ((), e), n, 0us
                | VoidRegister (f, r), n -> f ((), (r bsb.CPU)), n, 0us
                | Byte f, n -> f ni, n, 1us
                | ByteExtra (f, e), n -> f (ni, e), n, 1us
                | ByteRegister (f, r), n -> f (ni, (r bsb.CPU)), n, 1us
                | Short f, n -> f nni, n, 2us
                | ShortExtra (f, e), n -> f (nni, e), n, 2us
                | ShortRegister (f, r), n -> f (nni, (r bsb.CPU)), n, 2us
            
            // Pre instruction call PC increment
            let isb = { bsb with CPU = { bsb.CPU with PC = bsb.CPU.PC + 1us + pcd } }

            // Execute operation
            let (cycles, msb) =
                match operationFunc with
                | c, Some (f) -> c, f isb
                | c, None -> c, isb

            let fcpu =
                match msb.CPU.Interrupt with
                | Disable -> { msb.CPU with Interrupt = Disabled }
                | Enable -> { msb.CPU with Interrupt = Enabled }
                | _ -> msb.CPU

            // Transfrom state & return
            let fsb = { msb with CPU = fcpu }
            Some(cycles, fsb)
