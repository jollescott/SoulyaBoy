namespace SoulyaBoy.Core

module internal SBExecutor =
    let private N sb =
        fun () -> MmuIO.ReadByte sb.MMU (sb.CPU.PC + 1us)

    let private NN sb =
        fun () -> MmuIO.ReadShort sb.MMU (sb.CPU.PC + 1us)

    let private incrementPC pcd =
        fun sb -> { sb with CPU = { sb.CPU with PC = sb.CPU.PC + 1us + pcd } }

    let private executeOperation operation =
        fun sb ->
            match operation with
            | c, Some f -> (c, f sb)
            | c, None -> (c, sb)

    let private handleInterruptState =
        fun (cycles, sb) ->
            (cycles,
             match sb.CPU.Interrupt with
             | Disable -> { sb with CPU = { sb.CPU with Interrupt = Disabling } }
             | Disabling -> { sb with CPU = { sb.CPU with Interrupt = Disabled } }
             | Enable -> { sb with CPU = { sb.CPU with Interrupt = Enabling } }
             | Enabling -> { sb with CPU = { sb.CPU with Interrupt = Enabled } }
             | _ -> sb)

    let private runInterrupt =
        fun (cycles, sb) ->
            if sb.CPU.Interrupt <> SBCpuInterrupt.Disabled then
                let IF = MmuIO.ReadByte sb.MMU 0xFF0Fus
                let FF = MmuIO.ReadByte sb.MMU 0xFFFFus

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
            let operation, _, pcd =
                match instruction with
                | Const c, n -> c (), n, 0us
                | ConstExtra (c, e), n -> c ((), e), n, 0us
                | Void f, n -> f (), n, 0us
                | VoidExtra (f, e), n -> f ((), e), n, 0us
                | VoidRegister (f, r), n -> f ((), (r bsb.CPU)), n, 0us
                | Byte f, n -> f (ni ()), n, 1us
                | ByteExtra (f, e), n -> f (ni (), e), n, 1us
                | ByteRegister (f, r), n -> f (ni (), (r bsb.CPU)), n, 1us
                | Short f, n -> f (nni ()), n, 2us
                | ShortExtra (f, e), n -> f (nni (), e), n, 2us
                | ShortRegister (f, r), n -> f (nni (), (r bsb.CPU)), n, 2us

            Some(
                bsb
                |> incrementPC pcd
                |> executeOperation operation
                |> runInterrupt
                |> handleInterruptState
            )
