namespace SoulyaBoy.Core

type SBCpuInterrupt =
    | Disabled
    | Disabling
    | Disable
    | Enable
    | Enabling
    | Enabled

type SBCpu =
    { A: byte
      F: byte

      B: byte
      C: byte

      D: byte
      E: byte

      H: byte
      L: byte

      SP: uint16
      PC: uint16
      IE: byte;

      Interrupt: SBCpuInterrupt }

module SBCpuFactory =
    let internal CreateCPU =
        let b, c = SBUtils.toBytes 0x0013us
        let d, e = SBUtils.toBytes 0x00D8us
        let h, l = SBUtils.toBytes 0x014Dus 
        
        { A = 0x01uy
          F = 0xB0uy
          B = b
          C = c
          D = d
          E = e
          H = h
          L = l
          PC = 0x100us
          SP = 0xFFFEus
          IE = 0uy
          Interrupt = Enabled }
