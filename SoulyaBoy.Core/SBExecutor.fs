namespace SoulyaBoy.Core

module internal SBExecutor =

    let private sb = new SBBuilder()

    let private IncrementPC pcd = sb {
        let! mb = SB.Get
        do! SB.Put { mb with CPU = { mb.CPU with PC = mb.CPU.PC + 1us + pcd } } 
    }

    let private handleInterruptState cycles = sb {
        let! mb = SB.Get

        let handledMB = match mb.CPU.Interrupt with
                         | Disable -> { mb with CPU = { mb.CPU with Interrupt = Disabling } }
                         | Disabling -> { mb with CPU = { mb.CPU with Interrupt = Disabled } }
                         | Enable -> { mb with CPU = { mb.CPU with Interrupt = Enabling } }
                         | Enabling -> { mb with CPU = { mb.CPU with Interrupt = Enabled } }
                         | _ -> mb

        do! SB.Put handledMB
    }

    let private ReadOpcode = sb {
        let! mb = SB.Get
        return! SBIO.ReadByte mb.CPU.PC
    }

    let private RetrieveOpcodeInstruction opcode = sb {
        if SBOpcodes.INSTRUCTIONS.ContainsKey(opcode) then
            return SBOpcodes.INSTRUCTIONS[opcode]
        else 
            return! SB.Panic $"Instruction %X{opcode} is not implemented \n"
    } 

    let private ResolveOperation instruction = sb {
        let! mb = SB.Get

        // TODO: Limit to evaluation when needed? 
        let! byteImmediate = SBIO.ReadByte mb.CPU.PC 
        let! shortIntermediate = SBIO.ReadShort mb.CPU.PC

        let result = match instruction with
                        | Const c, n, cyc -> c (), n, 0us, cyc
                        | ConstExtra (c, e), n, cyc -> c ((), e), n, 0us, cyc
                        | Void f, n, cyc -> f (), n, 0us, cyc
                        | VoidExtra (f, e), n, cyc -> f ((), e), n, 0us, cyc
                        | VoidRegister (f, r), n, cyc -> f ((), (r mb.CPU)), n, 0us, cyc
                        | Byte f, n, cyc -> f (byteImmediate), n, 1us, cyc
                        | ByteExtra (f, e), n, cyc -> f (byteImmediate, e), n, 1us, cyc
                        | ByteRegister (f, r), n, cyc -> f (byteImmediate, (r mb.CPU)), n, 1us, cyc
                        | Short f, n, cyc -> f (shortIntermediate), n, 2us, cyc
                        | ShortExtra (f, e), n, cyc -> f (shortIntermediate, e), n, 2us, cyc
                        | ShortRegister (f, r), n, cyc -> f (shortIntermediate, (r mb.CPU)), n, 2us, cyc

        return result
    }


    let internal Execute = sb {
        do! SBGraphics.Process
        let! opcode = ReadOpcode
        let! instruction = RetrieveOpcodeInstruction opcode
        let! (operation, _, pcd, cycles) = ResolveOperation instruction
        
        do! operation
        do! IncrementPC pcd
        do! handleInterruptState cycles
    }
