namespace SoulyaBoy.Test

open NUnit.Framework
open SoulyaBoy.Core

module ByteLoads = 
    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let Test_LDH_n_A () =
         Util.createTestSB |> fun sb ->
            let n = 3uy
            let A = 12uy

            let opcode = SBOpcodes.ByteLoads.LD_n_A n
            let isb = { sb with CPU = { sb.CPU with A = A } }
            let msb = SBOpcodes.Execute opcode isb
            
            let memory = MmuIO.ReadByte msb.MMU (0xff00us + uint16(n))
            Assert.AreEqual(A, memory)
    
    