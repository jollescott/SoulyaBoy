namespace SoulyaBoy.Test

open NUnit.Framework
open SoulyaBoy.Core

module JumpTests =
    [<Test>]
    let Test_JP_nn () =
        Util.createTestSB
        |> fun mb ->
            let address = mb.CPU.PC + 3us
            let opcode = SBOpcodes.Jump.JP address

            let msb = SBOpcodes.Execute opcode mb

            Assert.AreEqual(address, msb.CPU.PC)

    [<Test>]
    let Test_JR_NZ_n () =
        Util.createTestSB
        |> fun mb ->
            let f = 0b1001_0000uy
            let pc = 2us
            let n = 3uy

            let isb = { mb with CPU = { mb.CPU with F = f; PC = pc } }

            let opcode = SBOpcodes.Jump.JR_NZ n
            let msb = SBOpcodes.Execute opcode isb

            Assert.AreEqual(pc, msb.CPU.PC)

        Util.createTestSB
        |> fun mb ->
            let f = 0b0110_0000uy
            let pc = 2us
            let n = 3uy

            let isb = { mb with CPU = { mb.CPU with F = f; PC = pc } }

            let opcode = SBOpcodes.Jump.JR_NZ n
            let msb = SBOpcodes.Execute opcode isb

            Assert.AreEqual(int pc + int (sbyte n), msb.CPU.PC)

    [<Test>]
    let Test_RST_n () =
        Util.createTestSB
        |> fun mb ->
            let arg = 0x18
            let pc = 200us

            let isb = { mb with CPU = { mb.CPU with PC = pc } }

            let opcode = SBOpcodes.Jump.RST((), arg)
            let msb = SBOpcodes.Execute opcode isb

            let stackTop = SBIO.ReadShort msb.MMU msb.CPU.SP

            Assert.AreEqual(pc, stackTop)
            Assert.AreEqual(arg, msb.CPU.PC)
