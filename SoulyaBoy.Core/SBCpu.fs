namespace SoulyaBoy.Core

open System.Runtime.CompilerServices

type SBCpuInterrupt =
    | Disabled
    | Disabling
    | Disable
    | Enable
    | Enabling
    | Enabled

[<Struct; IsReadOnly>]
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
      IE: byte
      IF: byte

      Stop: bool
      Halt: bool

      IME: SBCpuInterrupt }

module SBCpuFactory =
    let internal CreateCPU =
        let struct (b, c) = SBUtils.toBytes 0x0013us
        let struct (d, e) = SBUtils.toBytes 0x00D8us
        let struct (h, l) = SBUtils.toBytes 0x014Dus

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
          IF = 0uy
          Stop = false
          Halt = false
          IME = Enabled }
