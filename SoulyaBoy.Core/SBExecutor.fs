namespace SoulyaBoy.Core


module internal SBExecutor =
    let private N sb =
        fun () -> MmuIO.ReadByte sb.MMU (sb.CPU.PC + 1us)

    let private NN sb =
        fun () -> MmuIO.ReadShort sb.MMU (sb.CPU.PC + 1us)

    let private increment pcd =
        fun sb -> { sb with CPU = { sb.CPU with PC = sb.CPU.PC + 1us + pcd } }

    let private execute operationFunc =
        fun sb ->
            match operationFunc with
            | c, Some f -> (c, f sb)
            | c, None -> (c, sb)

    let private interrupt =
        fun (cycles, sb) ->
            let cpu =
                match sb.CPU.Interrupt with
                | Disable -> { sb.CPU with Interrupt = Disabled }
                | Enable -> { sb.CPU with Interrupt = Enabled }
                | _ -> sb.CPU

            (cycles, { sb with CPU = cpu })


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

            Some(bsb |> increment pcd |> execute operation |> interrupt)
