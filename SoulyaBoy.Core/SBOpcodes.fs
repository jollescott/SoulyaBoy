namespace SoulyaBoy.Core

// TODO: Cleanup SBImmediateValue & SBInstructionRegister
type internal SBImmediateValue<'a> = 'a
type internal SBInstructionProc<'a> = 'a -> SB<unit>
type internal SBInstructionRegister = SBInstructionProc<byte> * (SBCpu -> byte)
type internal SBInstructionExtra<'a> = SBInstructionProc<'a * int> * int

type internal SBInstruction =
    | Const of SBInstructionProc<unit>
    | ConstExtra of SBInstructionExtra<unit>
    | Void of SBInstructionProc<unit>
    | VoidExtra of SBInstructionExtra<unit>
    | Register of SBInstructionRegister
    | Byte of SBInstructionProc<SBImmediateValue<byte>>
    | ByteExtra of SBInstructionExtra<SBImmediateValue<byte>>
    | Short of SBInstructionProc<SBImmediateValue<uint16>>
    | ShortExtra of SBInstructionExtra<SBImmediateValue<uint16>>

type internal SBInstructionEntry = SBInstruction * string * int

type internal SBInstructionTable = Map<byte, SBInstructionEntry>

module internal SBOpcodes =

    let private sb = new SBBuilder()

    type Flags =
        | Z = 0b1000_0000uy
        | N = 0b0100_0000uy
        | H = 0b0010_0000uy
        | C = 0b0001_0000uy

    let Set flag = sb {
        let! mb = SB.Get
        do! SB.Put {mb with CPU = { mb.CPU with F = mb.CPU.F ||| (byte flag) }} 
    }   

    let Reset flag = sb {
        let! mb = SB.Get
        do! SB.Put { mb with CPU = { mb.CPU with F = mb.CPU.F &&& ~~~(byte flag) }}
    }

    let SetIf flag condition =
        if condition then Set flag else Reset flag

    let BitCarryCheck R r n = ((int R &&& (0b1 <<< n)) = (int r &&& (0b1 <<< 1)))

    module ByteLoads =

        let LD_n mut = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = mut mb}
        }

        let LD_A n = LD_n (fun mb -> { mb.CPU with A = n })
        let LD_B n = LD_n (fun mb -> { mb.CPU with B = n })
        let LD_C n = LD_n (fun mb -> { mb.CPU with C = n })
        let LD_D n = LD_n (fun mb -> { mb.CPU with D = n })
        let LD_E n = LD_n (fun mb -> { mb.CPU with E = n })
        let LD_H n = LD_n (fun mb -> { mb.CPU with H = n })

        let LD_FF00_C_A () = sb {
            let! mb = SB.Get
            let address = 0xFF00us + uint16 mb.CPU.C

            do! SBIO.WriteByte address mb.CPU.A
        }

        let LD_R_HL setR = sb {
            let! mb = SB.Get
            let address = SBUtils.toShort mb.CPU.H mb.CPU.L
            let! r = SBIO.ReadByte address
            do! SB.Put { mb with CPU = setR mb r }
        }

        let LD_D_HL () = LD_R_HL (fun mb d -> { mb.CPU with D = d })
        let LD_E_HL () = LD_R_HL (fun mb e -> { mb.CPU with E = e })
        let LD_H_HL () = LD_R_HL (fun mb h -> { mb.CPU with H = h })

        let LD_HLD_A () = sb {
            // TODO: Solve state changes within opcode
            // TODO: Move to LD (HL),A
            let! mb = SB.Get

            // TODO Move to DEC HL
            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let h, l = SBUtils.toBytes (hl - 1us)

            do! SB.Put { mb with CPU = { mb.CPU with H = h; L = l } }
            do! SBIO.WriteByte hl mb.CPU.A
        }

        let LD_A_HLI () = sb {
            let! mb = SB.Get

            //TODO: Move to INC HL
            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let h, l = SBUtils.toBytes (hl + 1us)

            // TODO: Move to LD A,(HL)
            let! a = SBIO.ReadByte hl
            do! SB.Put { mb with CPU = { mb.CPU with H = h; L = l; A = a}}
        }

        let LD_n_A n = sb {
            let! mb = SB.Get

            let address = 0xFF00us + uint16 n
            do! SBIO.WriteByte address mb.CPU.A
        }

        let LD_A_n n = sb {
            let! mb = SB.Get

            let address = 0xFF00us + uint16 n
            let! A = SBIO.ReadByte address

            do! SB.Put { mb with CPU = { mb.CPU with A = A } }
        }

        let LD_HL_n n = sb {
            let! mb = SB.Get

            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            do! SBIO.WriteByte hl n
        } 

        let LD_nn_A nn = sb {
            let! mb = SB.Get
            do! SBIO.WriteByte nn mb.CPU.A
        } 

    module ShortLoads =

        let LD_nn set nn = sb {
            let! mb = SB.Get
            let (h: byte, l: byte) = SBUtils.toBytes nn
            do! SB.Put { mb with CPU = set mb.CPU h l }
        }

        let LD_BC = LD_nn (fun cpu h l -> { cpu with B = h; C = l })
        let LD_HL = LD_nn (fun cpu h l -> { cpu with H = h; L = l })

        let LD_SP nn = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with SP = nn }}
        }

        let PUSH nn = sb {
            let! mb = SB.Get

            let sp = mb.CPU.SP - 2us            
            do! SB.Put { mb with CPU = { mb.CPU with SP = sp }} 
            do! SBIO.WriteShort sp nn
        } 

        let PUSH_DE () = sb {
            let! mb = SB.Get

            let de = SBUtils.toShort mb.CPU.D mb.CPU.E
            do! PUSH de
        }

        let POP = sb {
            let! mb = SB.Get

            let sp = mb.CPU.SP
            let! top = SBIO.ReadShort sp 

            do! SB.Put { mb with CPU = { mb.CPU with SP = sp + 2us }}
            return top
        }

        let POP_HL () = sb {
            let! mb = SB.Get

            let! top = POP
            let (h, l) = SBUtils.toBytes top
            do! SB.Put { mb with CPU = { mb.CPU with H = h; L = l }}
        }

    module Jump =
        let JP nn = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with PC = nn}}
        }

        let JP_HL () = sb {
            let! mb = SB.Get
            let address = SBUtils.toShort mb.CPU.H mb.CPU.L
            do! JP address
        }

        let JR_OP op flag n = sb {
            let! mb = SB.Get

            if op (mb.CPU.F &&& byte flag) 0uy then
                let address = uint16 (int mb.CPU.PC + int (sbyte n))
                return! JP address
        }

        let JR_N = JR_OP (<>)
        let JR = JR_OP (=)

        let JR_NZ = JR_N Flags.Z
        let JR_Z = JR Flags.Z
        let JR_NC = JR_N Flags.C

        let RST ((), arg) = sb {
            let! mb = SB.Get

            let address: uint16 = uint16 arg
            do! ShortLoads.PUSH mb.CPU.PC
            do! JP address
        }

    module Bit = 
        let RES get set b = sb {            
            let! mb = SB.Get

            let r = get mb.CPU
            let x = r &&& ~~~(1uy <<< int b)

            do! SB.Put { mb with CPU = set mb.CPU x }
        }

        let RES_A = RES (fun cpu -> cpu.A) (fun cpu x -> { cpu with A = x } )
            

    module Calls =
        let CALL nn = sb {
            let! mb = SB.Get
            do! ShortLoads.PUSH mb.CPU.PC 
            do! Jump.JP nn
        }

        let CALL_Z nn = sb {
            let! mb = SB.Get

            if mb.CPU.F &&& byte Flags.Z <> 0uy then
                do! CALL nn
        }

    module Returns = 
        let RET () = sb {
            let! address = ShortLoads.POP
            do! Jump.JP address
        }

    module ByteALU =

        let private IncFlags R r = sb {
            do! SetIf Flags.Z (r = 0uy)
            do! Reset Flags.N
            do! SetIf Flags.H ((R &&& 0b0100uy) <> (r &&& 0b0100uy))
        }

        // TODO: Merge with & parametrize DEC_n?
        let private INC_n getN setN = sb {
            let! mb = SB.Get
            let n = getN mb.CPU
            let x = n + 1uy

            do! SB.Put { mb with CPU = setN mb.CPU x}
            do! IncFlags n x
        }

        let INC_C () = INC_n (fun cpu -> cpu.C) (fun cpu x -> { cpu with C = x})

        let private DecFlags R r = sb {
            do! SetIf Flags.Z (r = 0uy)
            do! Set Flags.N
            do! SetIf Flags.H ((R &&& 0b1000uy) <> (r &&& 0b1000uy))
        }

        let private DEC_n getN setN = sb {            
            let! mb = SB.Get
            let n = getN mb.CPU
            let x = n - 1uy
            
            do! SB.Put { mb with CPU = setN mb.CPU x }
            do! DecFlags n x
        }

        let DEC_B () = DEC_n (fun cpu -> cpu.B) (fun cpu x -> { cpu with B = x })
        let DEC_C () = DEC_n (fun cpu -> cpu.C) (fun cpu x -> { cpu with C = x })
        let DEC_D () = DEC_n (fun cpu -> cpu.D) (fun cpu x -> { cpu with D = x })
        let DEC_E () = DEC_n (fun cpu -> cpu.E) (fun cpu x -> { cpu with E = x })

        let ADD_n n = sb {
            let! mb = SB.Get

            let a = mb.CPU.A + n
            do! SB.Put { mb with CPU = { mb.CPU with A = a }}

            do! SetIf Flags.Z (a = 0uy)
            do! Reset Flags.N
            do! SetIf Flags.H ((mb.CPU.A &&& 0b0100uy) <> (a &&& 0b0100uy))
            do! SetIf Flags.C ((mb.CPU.A &&& 0b0100_0000uy) <> (a &&& 0b0100_0000uy))
        }

        let ADC n = sb {
            let! mb = SB.Get

            let carry = (mb.CPU.F &&& byte Flags.C)
            let a = mb.CPU.A
            let r = a + n + carry

            do! SB.Put { mb with CPU = { mb.CPU with A = r}}

            do! SetIf Flags.Z (r = 0uy)
            do! Reset Flags.N
            do! SetIf Flags.H ((a &&& 0b100uy) <> (r &&& 0b100uy))
            do! SetIf Flags.C ((a &&& 0b0100_0000uy) <> (r &&& 0b100_0000uy))
        }

        let AND n = sb {
            let! mb = SB.Get

            let a = n &&& mb.CPU.A

            do! SB.Put { mb with CPU = { mb.CPU with A = a }}

            do! SetIf Flags.Z (a = 0uy)
            do! Reset Flags.N
            do! Set Flags.H
            do! Reset Flags.C
        }

        let OR rg = sb {
            let! mb = SB.Get

            let r = rg ||| mb.CPU.A

            do! SB.Put { mb with CPU = { mb.CPU with A = r }}

            do! SetIf Flags.Z (r = 0uy)
            do! Reset Flags.N
            do! Reset Flags.H
            do! Reset Flags.C
        }

        let XOR rg = sb {
            let! mb = SB.Get

            let r = rg ^^^ mb.CPU.A

            do! SB.Put {mb with CPU = { mb.CPU with A = r }}

            do! SetIf Flags.Z (r = 0uy)
            do! Reset Flags.N
            do! Reset Flags.H
            do! Reset Flags.C
        }

        let CP_n n = sb {
            let! mb = SB.Get

            let A = mb.CPU.A
            let r = A - n

            do! SetIf Flags.Z (r = 0uy)
            do! Set Flags.N
            // Unclear if borrow or "no" borrow.
            do! SetIf Flags.H ((A &&& 0b1000uy) = (r &&& 0b1000uy))
            do! SetIf Flags.C (A < n)
        }

    module ShortALU =
        let ADD_HL nn = sb {
            let! mb = SB.Get

            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let r = hl + nn
            let (h,l) = SBUtils.toBytes r

            do! SB.Put { mb with CPU = { mb.CPU with H = h; L = l }}

            do! Reset Flags.N
            do! SetIf Flags.H (BitCarryCheck hl r 11)
            do! SetIf Flags.C (BitCarryCheck hl r 15)
        }

        let ADD_HL_DE () = sb {
            let! mb = SB.Get

            let de = SBUtils.toShort mb.CPU.D mb.CPU.E
            do! ADD_HL de
        }

        let ADD_HL_SP () = sb {
            let! mb = SB.Get
            do! ADD_HL mb.CPU.SP
        }

        let INC_DEC_nn op get set = sb {
            let! mb = SB.Get
            
            let nn = get mb ||> SBUtils.toShort 
            let (h,l) = SBUtils.toBytes (op nn 1us)

            do! SB.Put { mb with CPU = set mb h l }
        }

        let INC_nn = INC_DEC_nn (+)

        let INC_BC () = INC_nn (fun mb -> (mb.CPU.B, mb.CPU.C)) (fun mb b c -> { mb.CPU with B = b; C = c }) 

        let INC_HL () = INC_nn (fun mb -> (mb.CPU.H, mb.CPU.L)) (fun mb h l -> { mb.CPU with H = h; L = l })

        let DEC_nn = INC_DEC_nn (-)

        let DEC_BC () = DEC_nn (fun mb -> (mb.CPU.B, mb.CPU.C)) (fun mb h l -> { mb.CPU with B = h; C = l })

    module Control =
        let NOP () = SB.Return ()

    module Misc =

        let SWAP_A () = sb {
            let! mb = SB.Get
            let a = mb.CPU.A

            let lNibble = a &&& 0b1111uy
            let hNibble = a >>> 4

            let s = (lNibble <<< 4) ||| hNibble

            do! SB.Put { mb with CPU = { mb.CPU with A = s }}

            do! SetIf Flags.Z (s = 0uy)
            do! Reset Flags.N
            do! Reset Flags.H
            do! Reset Flags.C
        }

        let CPL () = sb {
            let! mb = SB.Get
            let a = ~~~mb.CPU.A

            do! SB.Put { mb with CPU = { mb.CPU with A = a }}
            do! Set Flags.N
            do! Set Flags.H
        }

        let DI () = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with Interrupt = Disable } }
        }

        let IE () = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with Interrupt = Enable }}
        }

    module RotatesShifts =
        let RRA () = sb {
            let! mb = SB.Get
            
            let c: byte = mb.CPU.A &&& 0b1uy
            let a: byte = (mb.CPU.A >>> 1) ||| (c <<< 8)

            do! SB.Put { mb with CPU = { mb.CPU with A = a } }

            do! SetIf Flags.Z (a = 0uy)
            do! Reset Flags.N
            do! Reset Flags.H
            do! SetIf Flags.C (c = 1uy)
        }

    let internal INSTRUCTIONS =
        SBInstructionTable [ (0x3Euy, (Byte(ByteLoads.LD_A), "LD A,n", 8))
                             (0x78uy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.B)), "LD A,B", 4))
                             (0x79uy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.C)), "LD A,C", 4))
                             (0x06uy, (Byte(ByteLoads.LD_B), "LD B,n", 8))
                             (0x47uy, (Register(ByteLoads.LD_B, (fun cpu -> cpu.A)), "LD B,A", 4))
                             (0x40uy, (Register(ByteLoads.LD_B, (fun cpu -> cpu.B)), "LD B,B", 4))
                             (0x0Euy, (Byte(ByteLoads.LD_C), "LD C,n", 8))
                             (0x4Fuy, (Register(ByteLoads.LD_C, (fun cpu -> cpu.C)), "LD C,A", 4))
                             (0x16uy, (Byte(ByteLoads.LD_D), "LD D,n", 8))
                             (0x5Fuy, (Register(ByteLoads.LD_E, (fun cpu -> cpu.A)), "LD E,A", 4))
                             (0x60uy, (Register(ByteLoads.LD_H, (fun cpu -> cpu.B)), "LD H,B", 4))
                             (0xE2uy, (Void(ByteLoads.LD_FF00_C_A), "LD (C),A", 8))
                             (0x56uy, (Void(ByteLoads.LD_D_HL), "LD D,(HL)", 8))
                             (0x5Euy, (Void(ByteLoads.LD_E_HL), "LD E,(HL)", 8))
                             (0x66uy, (Void(ByteLoads.LD_H_HL), "LD H,(HL)", 8))
                             (0x32uy, (Void(ByteLoads.LD_HLD_A), "LD (HLD),A", 8))
                             (0x2Auy, (Void(ByteLoads.LD_A_HLI), "LDI A,(HL)", 8))
                             (0xE0uy, (Byte(ByteLoads.LD_n_A), "LDH (n),A", 12))
                             (0xF0uy, (Byte(ByteLoads.LD_A_n), "LDH A,(n)", 12))
                             (0x36uy, (Byte(ByteLoads.LD_HL_n), "LD (HL), n", 12))
                             (0xEAuy, (Short(ByteLoads.LD_nn_A), "LD (nn),A", 16))

                             (0x0Cuy, (Void(ByteALU.INC_C), "INC C", 4))
                             (0x05uy, (Void(ByteALU.DEC_B), "DEC B", 4))
                             (0x0Duy, (Void(ByteALU.DEC_C), "DEC C", 4))
                             (0x1Duy, (Void(ByteALU.DEC_E), "DEC E", 4))
                             (0x87uy, (Register(ByteALU.ADD_n, (fun cpu -> cpu.A)), "ADD A", 4))
                             (0xCEuy, (Byte(ByteALU.ADC), "ADC A,n", 8))
                             (0xE6uy, (Byte(ByteALU.AND), "AND #", 8))
                             (0xA1uy, (Register(ByteALU.AND, (fun cpu -> cpu.C)), "AND C", 8))
                             (0xB0uy, (Register(ByteALU.OR, (fun cpu -> cpu.B)), "OR B", 4))
                             (0xB1uy, (Register(ByteALU.OR, (fun cpu -> cpu.C)), "OR C", 4))
                             (0xAFuy, (Register(ByteALU.XOR, (fun cpu -> cpu.A)), "XOR A", 4))
                             (0xA9uy, (Register(ByteALU.XOR, (fun cpu -> cpu.C)), "XOR C", 4))
                             (0xFEuy, (Byte(ByteALU.CP_n), "CP #", 8))

                             (0x0uy, (Const(Control.NOP), "NOP", 4))

                             (0xC3uy, (Short(Jump.JP), "JP NN", 16))
                             (0xE9uy, (Void(Jump.JP_HL), "JP (HL)", 4))
                             (0x20uy, (Byte(Jump.JR_NZ), "JR NZ", 8))
                             (0x28uy, (Byte(Jump.JR_Z), "JR N", 8))
                             (0x30uy, (Byte(Jump.JR_NC), "JR NC", 8))
                             (0xEFuy, (VoidExtra(Jump.RST, 0x28), "RST 28H", 16))
                             (0xFFuy, (VoidExtra(Jump.RST, 0x38), "RST 38H", 16))

                             (0xCDuy, (Short(Calls.CALL), "CALL nn", 12))
                             (0xCCuy, (Short(Calls.CALL_Z), "CALL Z,nn", 12))

                             (0xC9uy, (Void(Returns.RET), "RET", 8))

                             (0x01uy, (Short(ShortLoads.LD_BC), "LD BC,nn", 12))
                             (0x21uy, (Short(ShortLoads.LD_HL), "LD HL,nn", 12))
                             (0x31uy, (Short(ShortLoads.LD_SP), "LD SP,nn", 12))
                             (0xD5uy, (Void(ShortLoads.PUSH_DE),"PUSH DE",  16))
                             (0xE1uy, (Void(ShortLoads.POP_HL), "POP HL",   12))

                             (0x19uy, (Void(ShortALU.ADD_HL_DE), "ADD HL,DE", 8))
                             (0x39uy, (Void(ShortALU.ADD_HL_SP), "ADD HL,SP", 8))
                             (0x03uy, (Void(ShortALU.INC_BC), "INC BC", 8))
                             (0x23uy, (Void(ShortALU.INC_HL), "INC HL", 8))
                             (0x0Buy, (Void(ShortALU.DEC_BC), "DEC BC", 8))

                             (0x2Fuy, (Void(Misc.CPL), "CPL", 4))
                             (0xF3uy, (Void(Misc.DI), "DI", 4))
                             (0xFBuy, (Void(Misc.IE), "IE", 4))

                             (0x1Fuy, (Void(RotatesShifts.RRA), "RRA", 4)) ]

    let internal CB_EXTENSIONS = 
        SBInstructionTable [ (0x87uy, (Byte(Bit.RES_A), "RES b,A", 8))
                             (0x37uy, (Void(Misc.SWAP_A), "SWAP A", 8))]
