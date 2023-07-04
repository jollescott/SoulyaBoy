namespace SoulyaBoy.Core

module internal SBExecutor =

    let private IncrementPC mb (operation, name, pcd) =
        Some ({ mb with CPU = { mb.CPU with PC = mb.CPU.PC + 1us + pcd } }, operation)

    let private executeOperation (mb, operation) =
        match operation with
            | c, Some f -> (c, f mb)
            | c, None -> (c, mb)
        |> Some

    let private handleInterruptState (cycles, mb) =
        Some (cycles,
             match mb.CPU.Interrupt with
             | Disable -> { mb with CPU = { mb.CPU with Interrupt = Disabling } }
             | Disabling -> { mb with CPU = { mb.CPU with Interrupt = Disabled } }
             | Enable -> { mb with CPU = { mb.CPU with Interrupt = Enabling } }
             | Enabling -> { mb with CPU = { mb.CPU with Interrupt = Enabled } }
             | _ -> mb)

(*
    let private runInterrupt =
        fun (cycles, mb) ->
            if mb.CPU.Interrupt <> Disabled then
                let IF = mb |> SBIO.ReadByte 0xFF0Fus
                let FF = mb |> SBIO.ReadByte 0xFFFFus

                if IF.IsNone || FF.IsNone

                let interruptByte = IF &&& FF

                let interruptAddress =
                    match interruptByte with
                    | interruptByte when interruptByte &&& 0b00001uy <> 0uy -> Some(0x40us)
                    | interruptByte when interruptByte &&& 0b00010uy <> 0uy -> Some(0x48us)
                    | interruptByte when interruptByte &&& 0b00100uy <> 0uy -> Some(0x50us)
                    | interruptByte when interruptByte &&& 0b01000uy <> 0uy -> Some(0x58us)
                    | interruptByte when interruptByte &&& 0b10000uy <> 0uy -> Some(0x60us)
                    | _ -> None

                if interruptAddress.IsSome then
                    (cycles,
                     mb
                     |> SBOpcodes.Execute(SBOpcodes.ShortLoads.PUSH mb.CPU.PC)
                     |> SBOpcodes.Execute(SBOpcodes.Jump.JP interruptAddress.Value))
                else
                    (cycles, mb)
            else
                (cycles, mb)
*)

    let private ReadOpcode mb = 
        mb |> (SBIO.ReadByte mb.CPU.PC)

    let private RetrieveOpcodeInstruction opcode = 
        if SBOpcodes.INSTRUCTIONS.ContainsKey(opcode) then
            let instruction = SBOpcodes.INSTRUCTIONS[opcode]
            Some instruction
        else 
            printf $"Instruction %X{opcode} is not implemented \n"
            None

    let private ResolveOperation mb instruction =
        let readByteIntermediate = SBIO.ReadByte mb.CPU.PC 
        let readShortIntermediate = SBIO.ReadShort mb.CPU.PC

        match instruction with
        | Const c, n -> c (), n, 0us
        | ConstExtra (c, e), n -> c ((), e), n, 0us
        | Void f, n -> f (), n, 0us
        | VoidExtra (f, e), n -> f ((), e), n, 0us
        | VoidRegister (f, r), n -> f ((), (r mb.CPU)), n, 0us
        | Byte f, n -> f (readByteIntermediate), n, 1us
        | ByteExtra (f, e), n -> f (readByteIntermediate, e), n, 1us
        | ByteRegister (f, r), n -> f (readByteIntermediate, (r mb.CPU)), n, 1us
        | Short f, n -> f (readShortIntermediate), n, 2us
        | ShortExtra (f, e), n -> f (readShortIntermediate, e), n, 2us
        | ShortRegister (f, r), n -> f (readShortIntermediate, (r mb.CPU)), n, 2us
        |> Some

    let Execute mb _ =
         SBGraphics.Process mb
        |> SBUtils.bind ReadOpcode
        |> SBUtils.bind RetrieveOpcodeInstruction
        |> SBUtils.bind (ResolveOperation mb)
        |> SBUtils.bind (IncrementPC mb)
        |> SBUtils.bind executeOperation
        |> SBUtils.bind handleInterruptState
