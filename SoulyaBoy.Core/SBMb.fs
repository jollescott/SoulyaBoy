namespace SoulyaBoy.Core

type SBMb =
    { CPU: SBCpu
      MMU: SBMmu
      GPU: SBGpu
      Joypad: byte }

module SBMbFactory =
    let CreateSBMb rom =
        { CPU = SBCpuFactory.CreateCPU
          MMU = SBMmuFactory.CreateMMU(rom)
          GPU = SBGpuFactory.CreateGPU
          Joypad = 0uy }
