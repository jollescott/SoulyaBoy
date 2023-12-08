namespace SoulyaBoy.Core

open System.Runtime.CompilerServices

[<Struct; IsReadOnly>]
type SBMb = { CPU: SBCpu; MMU: SBMmu; GPU: SBGpu }

module SBMbFactory =
    let CreateSBMb (rom) =
        { CPU = SBCpuFactory.CreateCPU
          MMU = SBMmuFactory.CreateMMU(rom)
          GPU = SBGpuFactory.CreateGPU }
