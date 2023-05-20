namespace SoulyaBoy.Core

module internal SBExecutor =

    let private IncrementPC sb (operation, name, pcd) =
        Some ({ sb with CPU = { sb.CPU with PC = sb.CPU.PC + 1us + pcd } }, operation)

    let private executeOperation (sb, operation) =
        match operation with
            | c, Some f -> (c, f sb)
            | c, None -> (c, sb)
        |> Some

    let private handleInterruptState (cycles, sb) =
        Some (cycles,
             match sb.CPU.Interrupt with
             | Disable -> { sb with CPU = { sb.CPU with Interrupt = Disabling } }
             | Disabling -> { sb with CPU = { sb.CPU with Interrupt = Disabled } }
             | Enable -> { sb with CPU = { sb.CPU with Interrupt = Enabling } }
             | Enabling -> { sb with CPU = { sb.CPU with Interrupt = Enabled } }
             | _ -> sb)

(*
    let private runInterrupt =
        fun (cycles, sb) ->
            if sb.CPU.Interrupt <> Disabled then
                let IF = sb |> SBIO.ReadByte 0xFF0Fus
                let FF = sb |> SBIO.ReadByte 0xFFFFus

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
                     sb
                     |> SBOpcodes.Execute(SBOpcodes.ShortLoads.PUSH sb.CPU.PC)
                     |> SBOpcodes.Execute(SBOpcodes.Jump.JP interruptAddress.Value))
                else
                    (cycles, sb)
            else
                (cycles, sb)
*)

    let private ReadOpcode sb = 
        sb |> (SBIO.ReadByte sb.CPU.PC)

    let private RetrieveOpcodeInstruction opcode = 
        if SBOpcodes.INSTRUCTIONS.ContainsKey(opcode) then
            let instruction = SBOpcodes.INSTRUCTIONS[opcode]
            Some instruction
        else 
            printf $"Instruction %X{opcode} is not implemented \n"
            None

    let private ResolveOperation sb instruction =
        let readByteIntermediate = SBIO.ReadByte sb.CPU.PC 
        let readShortIntermediate = SBIO.ReadShort sb.CPU.PC

        match instruction with
        | Const c, n -> c (), n, 0us
        | ConstExtra (c, e), n -> c ((), e), n, 0us
        | Void f, n -> f (), n, 0us
        | VoidExtra (f, e), n -> f ((), e), n, 0us
        | VoidRegister (f, r), n -> f ((), (r sb.CPU)), n, 0us
        | Byte f, n -> f (readByteIntermediate), n, 1us
        | ByteExtra (f, e), n -> f (readByteIntermediate, e), n, 1us
        | ByteRegister (f, r), n -> f (readByteIntermediate, (r sb.CPU)), n, 1us
        | Short f, n -> f (readShortIntermediate), n, 2us
        | ShortExtra (f, e), n -> f (readShortIntermediate, e), n, 2us
        | ShortRegister (f, r), n -> f (readShortIntermediate, (r sb.CPU)), n, 2us
        |> Some

    let Execute sb _ =
         SBGraphics.Process sb
        |> SBUtils.bind ReadOpcode
        |> SBUtils.bind RetrieveOpcodeInstruction
        |> SBUtils.bind (ResolveOperation sb)
        |> SBUtils.bind (IncrementPC sb)
        |> SBUtils.bind executeOperation
        |> SBUtils.bind handleInterruptState
