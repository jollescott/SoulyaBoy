namespace SoulyaBoy.Core

open System

module internal SBExecutor =

    let private sb = SBBuilder()

    let private INTERRUPT_ADDRESSES = Map([
        (0, 0x0040us)
    ])

    let private IncrementPC pcd = sb {
        let! mb = SB.Get
        do! SB.Put { mb with CPU = { mb.CPU with PC = mb.CPU.PC + 1us + pcd } } 
    }

    let private UpdateIMEFlag = sb {
        let! mb = SB.Get

        let handledMB = match mb.CPU.IME with
                         | Disable -> { mb with CPU = { mb.CPU with IME = Disabling } }
                         | Disabling -> { mb with CPU = { mb.CPU with IME = Disabled } }
                         | Enable -> { mb with CPU = { mb.CPU with IME = Enabling } }
                         | Enabling -> { mb with CPU = { mb.CPU with IME = Enabled } }
                         | _ -> mb

        do! SB.Put handledMB
    }

    let rec private TryRunInterrupt i IE IF = sb {
        if int IE >>> i <> 0 && int IF >>> i <> 0 then
            let! mb = SB.Get
            do! SB.Put {mb with CPU = { mb.CPU with IF = IF &&& ~~~(1uy<<<i) }}       

            do! SBOpcodes.Calls.CALL INTERRUPT_ADDRESSES[i]
            printf $"Executing interrupt at address %X{INTERRUPT_ADDRESSES[i]}\n"
        elif i < 4 then
            do! TryRunInterrupt (i+1) IE IF
    }

    let private HandleInterrupts = sb {
        let! mb = SB.Get

        let IE = mb.CPU.IE
        let IF = mb.CPU.IF

        if(mb.CPU.IME = Enabled) && IE <> 0uy && IF <> 0uy then
            do! SB.Put { mb with CPU = { mb.CPU with IME = Disabled }}
            do! TryRunInterrupt 0 IE IF
    }

    let private RetrieveOpcodeInstruction opcode (instructions: SBInstructionTable) = sb {
        if instructions.ContainsKey(opcode) <> true then
            printf $"Instruction not implemented %X{opcode}\n"
        
        return instructions[opcode]
    }

    let private ReadOpcode = sb {
        let! mb = SB.Get
        return! SBIO.ReadByte mb.CPU.PC
    }

    let private FetchInstruction = sb {
        let! opcode = ReadOpcode
        let cbExtension = opcode = 0xCBuy

        if cbExtension then
            do! IncrementPC 0us
            let! exOpcode = ReadOpcode
            return! RetrieveOpcodeInstruction exOpcode SBOpcodes.CB_EXTENSIONS
        else 
            return! RetrieveOpcodeInstruction opcode SBOpcodes.INSTRUCTIONS
    } 

    let private ResolveOperation instruction = sb {
        let! mb = SB.Get

        // TODO: Limit to evaluation when needed? 
        let! byteImmediate = SBIO.ReadByte (mb.CPU.PC + 1us) 
        let! shortIntermediate = SBIO.ReadShort (mb.CPU.PC + 1us)

        let result = match instruction with
                        | Const c, n, cyc -> c (), n, 0us, cyc
                        | ConstExtra (c, e), n, cyc -> c ((), e), n, 0us, cyc
                        | Void f, n, cyc -> f (), n, 0us, cyc
                        | VoidExtra (f, e), n, cyc -> f ((), e), n, 0us, cyc
                        | Byte f, n, cyc -> f (byteImmediate), n, 1us, cyc
                        | ByteExtra (f, e), n, cyc -> f (byteImmediate, e), n, 1us, cyc
                        | Short f, n, cyc -> f (shortIntermediate), n, 2us, cyc
                        | ShortExtra (f, e), n, cyc -> f (shortIntermediate, e), n, 2us, cyc
                        | Register (f, r), n, cyc -> f (r mb.CPU), n, 0us, cyc

        return result
    }

    let GetStop = sb {
        let! mb = SB.Get
        return mb.CPU.Stop
    }

    let internal Run pixelPipe = sb {
        let! stop = GetStop

        if not stop then 
            do! UpdateIMEFlag
            do! HandleInterrupts

            let! instruction = FetchInstruction
            let! (operation, _, pcd, _) = ResolveOperation instruction

            do! IncrementPC pcd
            do! operation

            do! SBGraphics.Process pixelPipe
            
        return! SB.Get
    }
