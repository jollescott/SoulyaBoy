namespace SoulyaBoy.Test

open NUnit.Framework
open SoulyaBoy.Core

module ByteALUTests =
    [<SetUp>]
    let Setup () = ()

    [<Test>]
    let Test_CP_n_zero () =
        Util.createTestSB
        |> fun mb ->
            let n = 12uy
            let A = 12uy

            let opcode = SBOpcodes.ByteALU.CP_n n
            let isb = { mb with CPU = { mb.CPU with A = A } }
            let msb = SBOpcodes.Execute opcode isb

            Assert.AreEqual(0b1100_0000uy, msb.CPU.F)

    [<Test>]
    let Test_CP_n_carry () =
        Util.createTestSB
        |> fun mb ->
            let n = 12uy
            let A = 6uy

            let opcode = SBOpcodes.ByteALU.CP_n n
            let isb = { mb with CPU = { mb.CPU with A = A } }
            let msb = SBOpcodes.Execute opcode isb

            Assert.AreEqual(0b0101_0000uy, msb.CPU.F)
