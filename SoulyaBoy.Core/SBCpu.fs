namespace SoulyaBoy.Core

type SBCpuInterrupt =
    | Disabled
    | Disable
    | Enable
    | Enabled

type SBCpu = { 
    A: byte;
    F: byte;
    
    B: byte;
    C: byte;
    
    D: byte;
    E: byte;

    H: byte;
    L: byte;

    SP: uint16;
    PC: uint16;

    Interupt: SBCpuInterrupt;
}

module SBCpuFactory = 
    let internal CreateCPU() = 
        { A = 0b1uy; F = 0b0uy; B = 0xFFuy; C = 0x13uy; D = 0b0uy; E = 0xC1uy; H = 0x84uy; L = 0x03uy; PC = 0x100us; SP = 0xFFFEus; Interupt = Disabled }