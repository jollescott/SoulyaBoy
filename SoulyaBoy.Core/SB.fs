namespace SoulyaBoy.Core

type SB = { CPU: SBCpu; MMU: SBMmu; GPU: SBGpu }

module SBFactory =
    let CreateSB (rom) =
        { CPU = SBCpuFactory.CreateCPU
          MMU = SBMmuFactory.CreateMMU(rom)
          GPU = SBGpuFactory.CreateGPU }
