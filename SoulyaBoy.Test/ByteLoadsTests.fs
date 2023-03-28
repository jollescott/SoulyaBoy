namespace SoulyaBoy.Test

open NUnit.Framework
open SoulyaBoy.Core

module ByteLoadsTests =
    [<SetUp>]
    let Setup () = ()

    [<Test>]
    let Test_LDH_n_A () =
        Util.createTestSB
        |> fun sb ->
            let n = 3uy
            let A = 12uy

            let opcode = SBOpcodes.ByteLoads.LD_n_A n
            let isb = { sb with CPU = { sb.CPU with A = A } }
            let msb = SBOpcodes.Execute opcode isb

            let memory = MmuIO.ReadByte msb.MMU (0xFF00us + uint16 n)
            Assert.AreEqual(A, memory)
            
    [<Test>]
    let Test_LDH_A_n () =
        Util.createTestSB
        |> fun sb ->
            let n = 3uy
            let memory = 12uy
            MmuIO.WriteByte sb.MMU (0xFF00us + uint16 n) memory
            
            let opcode = SBOpcodes.ByteLoads.LD_A_n n
            let msb = SBOpcodes.Execute opcode sb
            
            Assert.AreEqual(memory, msb.CPU.A)