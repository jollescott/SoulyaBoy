namespace SoulyaBoy.Core

type SBMb =
    { CPU: SBCpu
      GPU: SBGpu
      Joypad: byte }

module SBMbFactory =
    let CreateSBMb =
        { CPU = SBCpuFactory.CreateCPU
          GPU = SBGpuFactory.CreateGPU
          Joypad = 0uy }
