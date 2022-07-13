namespace SoulyaBoy.Core

type SB = { 
    CPU: SBCpu;
    MMU: SBMmu
}

module SBFactory = 
    let CreateSB(rom) = 
        { CPU = SBCpuFactory.CreateCPU(); MMU = SBMmuFactory.CreateMMU(rom); }