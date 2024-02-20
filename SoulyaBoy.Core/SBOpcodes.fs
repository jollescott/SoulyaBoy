namespace SoulyaBoy.Core

open Microsoft.FSharp.Collections

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
        do! SB.Put {mb with CPU = { mb.CPU with F = mb.CPU.F ||| byte flag }} 
    }   

    let Reset flag = sb {
        let! mb = SB.Get
        do! SB.Put { mb with CPU = { mb.CPU with F = mb.CPU.F &&& ~~~(byte flag) }}
    }

    let SetIf flag condition =
        if condition then Set flag else Reset flag


    module ShortALU =
        let ADD_HL nn = sb {
            let! mb = SB.Get

            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let r = int hl + int nn
            let struct (h,l) = SBUtils.toBytes (uint16 r)

            do! SB.Put { mb with CPU = { mb.CPU with H = h; L = l }}

            do! Reset Flags.N
            do! SetIf Flags.H ((hl &&& 0xFFFus) + (nn &&& 0xFFFus) > 0xFFFus)
            do! SetIf Flags.C (r > 0xFFFF)
        }
        
        let ADD_HL_BC () = sb {
            let! mb = SB.Get
            
            let bc = SBUtils.toShort mb.CPU.B mb.CPU.C
            do! ADD_HL bc
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
            let struct (h,l) = SBUtils.toBytes (op nn 1us)

            do! SB.Put { mb with CPU = set mb h l }
        }

        let INC_nn = INC_DEC_nn (+)
        let INC_BC () = INC_nn (fun mb -> (mb.CPU.B, mb.CPU.C)) (fun mb b c -> { mb.CPU with B = b; C = c }) 
        let INC_DE () = INC_nn (fun mb -> (mb.CPU.D, mb.CPU.E)) (fun mb d e -> { mb.CPU with D = d; E = e; }) 
        let INC_HL () = INC_nn (fun mb -> (mb.CPU.H, mb.CPU.L)) (fun mb h l -> { mb.CPU with H = h; L = l })

        let INC_SP () = sb {
            let! mb = SB.Get
            let sp = mb.CPU.SP + 1us
            do! SB.Put { mb with CPU = { mb.CPU with SP = sp }}
        }

        let DEC_nn = INC_DEC_nn (-)
        let DEC_BC () = DEC_nn (fun mb -> (mb.CPU.B, mb.CPU.C)) (fun mb b c -> { mb.CPU with B = b; C = c })
        let DEC_DE () = DEC_nn (fun mb -> (mb.CPU.D, mb.CPU.E)) (fun mb d e -> { mb.CPU with D = d; E = e })
        let DEC_HL () = DEC_nn (fun mb -> (mb.CPU.H, mb.CPU.L)) (fun mb h l -> { mb.CPU with H = h; L = l })

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
        let LD_L n = LD_n (fun mb -> { mb.CPU with L = n })

        let LD_A_nn nn = sb {
            let! a = SBIO.ReadByte nn
            do! LD_A a
        }

        let LD_A_BC () = sb {
            let! mb = SB.Get
            
            let address = SBUtils.toShort mb.CPU.B mb.CPU.C
            do! LD_A_nn address
        }
        
        let LD_A_DE () = sb {
            let! mb = SB.Get

            let address = SBUtils.toShort mb.CPU.D mb.CPU.E
            do! LD_A_nn address
        }

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

        let LD_A_HL () = LD_R_HL (fun mb a -> { mb.CPU with A = a })
        let LD_B_HL () = LD_R_HL (fun mb b -> { mb.CPU with B = b })
        let LD_C_HL () = LD_R_HL (fun mb c -> { mb.CPU with C = c })
        let LD_D_HL () = LD_R_HL (fun mb d -> { mb.CPU with D = d })
        let LD_E_HL () = LD_R_HL (fun mb e -> { mb.CPU with E = e })
        let LD_H_HL () = LD_R_HL (fun mb h -> { mb.CPU with H = h })

        let LD_nn_A nn = sb {
            let! mb = SB.Get
            do! SBIO.WriteByte nn mb.CPU.A
        } 

        let LD_DE_A () = sb {
            let! mb = SB.Get
            let address = SBUtils.toShort mb.CPU.D mb.CPU.E
            do! LD_nn_A address
        }


        let LD_HL_A () = sb {
            let! mb = SB.Get
            let address = SBUtils.toShort mb.CPU.H mb.CPU.L
            do! LD_nn_A address
        }

        let LDI_HL_A () = sb {
            do! LD_HL_A ()
            do! ShortALU.INC_HL ()
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
        
        let LDD_A_HL () = sb {
            do! LD_A_HL ()
            do! ShortALU.DEC_HL ()
        }

        let LDD_HL_A () = sb {
            do! LD_HL_A ()
            do! ShortALU.DEC_HL ()
        }

        let LDI_A_HL () = sb {
            let! mb = SB.Get

            //TODO: Move to INC HL
            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let struct(h, l) = SBUtils.toBytes (hl + 1us)

            // TODO: Move to LD A,(HL)
            let! a = SBIO.ReadByte hl
            do! SB.Put { mb with CPU = { mb.CPU with H = h; L = l; A = a}}
        }

    module ShortLoads =

        let LD_nn set nn = sb {
            let! mb = SB.Get
            let struct (h: byte, l: byte) = SBUtils.toBytes nn
            do! SB.Put { mb with CPU = set mb.CPU h l }
        }

        let LD_BC = LD_nn (fun cpu b c -> { cpu with B = b; C = c })
        let LD_DE = LD_nn (fun cpu d e -> { cpu with D = d; E = e })
        let LD_HL = LD_nn (fun cpu h l -> { cpu with H = h; L = l })

        let LD_SP nn = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with SP = nn }}
        }

        let LD_nn_SP nn = sb {
            let! mb = SB.Get
            do! SBIO.WriteShort nn mb.CPU.SP
        }

        let PUSH nn = sb {
            let! mb = SB.Get

            do! SBIO.WriteShort (mb.CPU.SP - 1us) nn

            let sp = mb.CPU.SP - 2us            
            do! SB.Put { mb with CPU = { mb.CPU with SP = sp }} 
        } 

        let PUSH_AF () = sb {
            let! mb = SB.Get 

            let af = SBUtils.toShort mb.CPU.A mb.CPU.F
            do! PUSH af
        }

        let PUSH_BC () = sb {
            let! mb = SB.Get

            let bc = SBUtils.toShort mb.CPU.B mb.CPU.C
            do! PUSH bc
        }

        let PUSH_DE () = sb {
            let! mb = SB.Get

            let de = SBUtils.toShort mb.CPU.D mb.CPU.E
            do! PUSH de
        }

        let PUSH_HL () = sb {
            let! mb = SB.Get

            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            do! PUSH hl
        }

        let POP = sb {
            let! mb = SB.Get

            let! top = SBIO.ReadShort (mb.CPU.SP + 1us) 

            let sp = mb.CPU.SP
            do! SB.Put { mb with CPU = { mb.CPU with SP = sp + 2us }}

            return top
        }

        let POP_AF () = sb {
            let! top = POP
            let struct (a, f) = SBUtils.toBytes top

            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with A = a; F = f &&& 0xF0uy }}
        }

        let POP_BC () = sb {
            let! top = POP
            let struct (b, c) = SBUtils.toBytes top

            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with B = b; C = c }}
        }

        let POP_DE () = sb {
            let! top = POP
            let struct (d, e) = SBUtils.toBytes top

            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with D = d; E = e }}
        }

        let POP_HL () = sb {
            let! top = POP
            let struct (h, l) = SBUtils.toBytes top
            
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with H = h; L = l }}
        }

    module Jump =
        let JP nn = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with PC = nn}}
        }

        let JP_OP op flag nn = sb {
            let! mb = SB.Get

            if op (mb.CPU.F &&& byte flag) 0uy then
                do! JP nn
        }

        let JP_N = JP_OP (=) 
        let JP_ = JP_OP (<>)

        let JP_NZ nn = JP_N Flags.Z nn
        let JP_Z nn = JP_ Flags.Z nn

        let JP_HL () = sb {
            let! mb = SB.Get
            let address = SBUtils.toShort mb.CPU.H mb.CPU.L
            do! JP address
        }

        let JR n = sb {
            let! mb = SB.Get

            let address = uint16 (int mb.CPU.PC + int (sbyte n))
            do! JP address
        }

        let JR_OP op flag n = sb {
            let! mb = SB.Get

            if op (mb.CPU.F &&& byte flag) 0uy then
                do! JR n
        }

        // Reset
        let JR_N = JR_OP (=)
        // Set
        let JR_ = JR_OP (<>)

        let JR_NZ = JR_N Flags.Z
        let JR_Z = JR_ Flags.Z
        let JR_NC = JR_N Flags.C
        let JR_C = JR_ Flags.C

        let RST ((), arg) = sb {
            let! mb = SB.Get

            let address: uint16 = uint16 arg
            do! ShortLoads.PUSH mb.CPU.PC
            do! JP address
        }

    module Bit =
        let RES_OP r b = r &&& ~~~(1uy <<< int b)
        
        let RES getR setR b = sb {            
            let! mb = SB.Get

            let r = getR mb.CPU
            let x = RES_OP r b

            do! SB.Put { mb with CPU = setR mb.CPU x }
        }

        let RES_0_A () = RES (fun cpu -> cpu.A) (fun cpu x -> { cpu with A = x }) 0
        
        let RES_HL b = sb {
            let! mb = SB.Get
            
            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let! r = SBIO.ReadByte hl 
            
            let x = RES_OP r b
           
            do! SBIO.WriteByte hl x
        }
        
        let RES_0_HL () = RES_HL 0
        
        let SET get set b = sb {
            let! mb = SB.Get
            
            let r = get mb.CPU
            let x = r ||| (1uy <<< int b)
            
            do! SB.Put { mb with CPU = set mb.CPU x}
        }
        
        let SET_6_A () = SET (fun cpu -> cpu.A) (fun cpu x -> { cpu with A = x }) 6uy

        let BIT_OP r b = r &&& (1uy <<< int b)
        
        let BIT get b = sb {
            let! mb = SB.Get
            
            let r = get mb.CPU
            let x = BIT_OP r b
            
            do! SetIf Flags.Z (x = 0uy)
            do! Reset Flags.N
            do! Set Flags.H
        }
        
        let BIT_2_B () = BIT (fun cpu -> cpu.B) 2
        let BIT_3_B () = BIT (fun cpu -> cpu.B) 3
        let BIT_4_B () = BIT (fun cpu -> cpu.B) 4
        let BIT_5_B () = BIT (fun cpu -> cpu.B) 5
        let BIT_7_A () = BIT (fun cpu -> cpu.A) 7
    
        let BIT_HL b = sb {
            let! mb = SB.Get
            
            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let! r = SBIO.ReadByte hl
            
            let x = BIT_OP r b
            
            do! SBIO.WriteByte hl x
        }
        
        let BIT_7_HL () = BIT_HL 7
    
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

        let SCF () = sb {
            do! Reset Flags.N
            do! Reset Flags.H
            do! Set Flags.C
        }

        let DAA () = sb {
            printf "TODO: Implement DAA later. \n"
        }

        let CPL () = sb {
            let! mb = SB.Get
            let a = ~~~mb.CPU.A

            do! SB.Put { mb with CPU = { mb.CPU with A = a }}
            do! Set Flags.N
            do! Set Flags.H
        }

        let STOP () = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with Stop = true }}
        }

        let DI () = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with IME = Disable } }
        }

        let IE () = sb {
            let! mb = SB.Get
            do! SB.Put { mb with CPU = { mb.CPU with IME = Enable }}
        }

    module Returns = 
        let RET () = sb {
            let! address = ShortLoads.POP
            do! Jump.JP address
        }

        let RET_OP op flag = sb {
            let! mb = SB.Get

            if op (mb.CPU.F &&& byte flag) 0uy then
                do! RET ()
        }

        let RET_N = RET_OP (=) 
        let RET_ = RET_OP (<>)

        let RET_NZ () = RET_N Flags.Z
        let RET_Z () = RET_ Flags.Z
        let RET_NC () = RET_N Flags.C
        let RET_C () = RET_ Flags.C
        
        let RETI () = sb {
            do! Misc.IE ()
            do! RET ()
        }

    module ByteALU =

        let private IncFlags R r = sb {
            do! SetIf Flags.Z (r = 0)
            do! Reset Flags.N
            do! SetIf Flags.H (R &&& 0xFuy = 0xFuy)
        }

        // TODO: Merge with & parametrize DEC_n?
        let private INC_n getN setN = sb {
            let! mb = SB.Get
            let n = getN mb.CPU
            let x = int n + 1

            do! SB.Put { mb with CPU = setN mb.CPU (byte x)}
            do! IncFlags n x
        }

        let INC_A () = INC_n (fun cpu -> cpu.A) (fun cpu a -> { cpu with A = a})
        let INC_C () = INC_n (fun cpu -> cpu.C) (fun cpu c -> { cpu with C = c })
        let INC_D () = INC_n (fun cpu -> cpu.D) (fun cpu d -> { cpu with D = d })
        let INC_E () = INC_n (fun cpu -> cpu.E) (fun cpu e -> { cpu with E = e })
        let INC_H () = INC_n (fun cpu -> cpu.H) (fun cpu h -> { cpu with H = h })
        let INC_L () = INC_n (fun cpu -> cpu.L) (fun cpu l -> { cpu with L = l })

        let private DecFlags R r = sb {
            do! SetIf Flags.Z (r = 0)
            do! Set Flags.N
            do! SetIf Flags.H (R &&& 0xFuy = 0uy)
        }

        let private DEC_n getN setN = sb {            
            let! mb = SB.Get
            let n = getN mb.CPU
            let x = int n - 1
            
            do! SB.Put { mb with CPU = setN mb.CPU (byte x) }
            do! DecFlags n x
        }

        let DEC_A () = DEC_n (fun cpu -> cpu.A) (fun cpu x -> { cpu with A = x })
        let DEC_B () = DEC_n (fun cpu -> cpu.B) (fun cpu x -> { cpu with B = x })
        let DEC_C () = DEC_n (fun cpu -> cpu.C) (fun cpu x -> { cpu with C = x })
        let DEC_D () = DEC_n (fun cpu -> cpu.D) (fun cpu x -> { cpu with D = x })
        let DEC_E () = DEC_n (fun cpu -> cpu.E) (fun cpu x -> { cpu with E = x })
        let DEC_L () = DEC_n (fun cpu -> cpu.L) (fun cpu x -> { cpu with L = x })

        let INC_DEC_addr_HL op opFlags = sb {
            let! mb = SB.Get

            let addr = SBUtils.toShort mb.CPU.H mb.CPU.L
            let! R = SBIO.ReadByte addr
            let r = op (int R) 1

            do! SBIO.WriteByte addr (byte r)
            do! opFlags R r
        }

        let INC_addr_HL () = INC_DEC_addr_HL (+) IncFlags
        let DEC_addr_HL () = INC_DEC_addr_HL (-) DecFlags

        let ADD_n n = sb {
            let! mb = SB.Get

            let a = int mb.CPU.A + int n
            do! SB.Put { mb with CPU = { mb.CPU with A = byte a }}

            do! SetIf Flags.Z (a = 0)
            do! Reset Flags.N
            do! SetIf Flags.H ((int mb.CPU.A &&& 0xF) + (int n &&& 0xF) > 0xF)
            do! SetIf Flags.C (a > 0xFF)
        }

        let ADD_HL () = sb {
            let! mb = SB.Get
            
            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let! n = SBIO.ReadByte hl

            do! ADD_n n
        }

        let ADC n = sb {
            let! mb = SB.Get

            let carry = (mb.CPU.F &&& byte Flags.C)
            let a = mb.CPU.A
            let r = int a + int n + int carry

            do! SB.Put { mb with CPU = { mb.CPU with A = byte r}}

            do! SetIf Flags.Z (r = 0)
            do! Reset Flags.N
            do! SetIf Flags.H (int mb.CPU.A &&& 0xF + int n &&& 0xF > 0xF)
            do! SetIf Flags.C (r > 0xFF)
        }

        let ADC_HL () = sb {
            let! mb = SB.Get
            
            let hl = SBUtils.toShort mb.CPU.H mb.CPU.L
            let! n = SBIO.ReadByte hl

            do! ADC n
        }

        let AND n = sb {
            let! mb = SB.Get

            let a = mb.CPU.A &&& n

            do! SB.Put { mb with CPU = { mb.CPU with A = a }}

            do! SetIf Flags.Z (a = 0uy)
            do! Reset Flags.N
            do! Set Flags.H
            do! Reset Flags.C
        }

        let OR rg = sb {
            let! mb = SB.Get

            let r = mb.CPU.A ||| rg

            do! SB.Put { mb with CPU = { mb.CPU with A = r }}

            do! SetIf Flags.Z (r = 0uy)
            do! Reset Flags.N
            do! Reset Flags.H
            do! Reset Flags.C
        }

        let XOR rg = sb {
            let! mb = SB.Get

            let r = mb.CPU.A ^^^ rg

            do! SB.Put {mb with CPU = { mb.CPU with A = r }}

            do! SetIf Flags.Z (r = 0uy)
            do! Reset Flags.N
            do! Reset Flags.H
            do! Reset Flags.C
        }

        let CP_n n = sb {
            let! mb = SB.Get

            let A = mb.CPU.A
            let r = int A - int n

            do! SetIf Flags.Z (r = 0)
            do! Set Flags.N
            // Unclear if borrow or "no" borrow.
            do! SetIf Flags.H ((int A &&& 0xF) < (int n &&& 0xF))
            do! SetIf Flags.C (A < n)
        }

    module Control =
        let NOP () = sb { () }

    module RotatesShifts =
        let RLCA () = sb {
            let! mb = SB.Get
            
            // TODO: Move to param when implementing RLA
            let c = mb.CPU.A >>> 7
            let a = ((mb.CPU.A <<< 1) &&& ~~~0b1uy) ||| c
            
            do! SB.Put { mb with CPU = { mb.CPU with A = a } }
            
            do! SetIf Flags.Z (a = 0uy)
            do! Reset Flags.N
            do! Reset Flags.H
            do! SetIf Flags.C (c = 1uy)
        }
        let RRA () = sb {
            let! mb = SB.Get
            
            // TODO: Move to param when implementing RRCA 
            let oc = mb.CPU.F &&& 0x20uy
            let nc = mb.CPU.A &&& 0b1uy
            let a = (mb.CPU.A >>> 1) ||| (oc <<< 7)

            do! SB.Put { mb with CPU = { mb.CPU with A = a } }

            do! SetIf Flags.Z (a = 0uy)
            do! Reset Flags.N
            do! Reset Flags.H
            do! SetIf Flags.C (nc = 1uy)
        }
        
        let SLA_A () = sb {
            let! mb = SB.Get
            
            let c = mb.CPU.A <<< 7
            let a = mb.CPU.A &&& ~~~0b1uy
            
            do! SB.Put { mb with CPU = { mb.CPU with A = a }}
            
            do! SetIf Flags.Z (a = 0uy)
            do! Set Flags.N
            do! Set Flags.H
            do! SetIf Flags.C (c = 1uy)
        }

    let internal INSTRUCTIONS =
        SBInstructionTable [ (0x3Euy, (Byte(ByteLoads.LD_A), "LD A,n", 8))
                             (0x7Fuy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.A)), "LD A,A", 4))
                             (0x78uy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.B)), "LD A,B", 4))
                             (0x79uy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.C)), "LD A,C", 4))
                             (0x7Auy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.D)), "LD A,D", 4))
                             (0x7Buy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.E)), "LD A,E", 4))
                             (0x7Cuy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.H)), "LD A,H", 4))
                             (0x7Duy, (Register(ByteLoads.LD_A, (fun cpu -> cpu.L)), "LD A,L", 4))
                             (0x0Auy, (Void(ByteLoads.LD_A_BC), "LD A,(BC)", 8))
                             (0x1Auy, (Void(ByteLoads.LD_A_DE), "LD A,(DE)", 8))
                             (0xFAuy, (Short(ByteLoads.LD_A_nn), "LD A,(nn)", 16))
                             (0x06uy, (Byte(ByteLoads.LD_B), "LD B,n", 8))
                             (0x47uy, (Register(ByteLoads.LD_B, (fun cpu -> cpu.A)), "LD B,A", 4))
                             (0x40uy, (Register(ByteLoads.LD_B, (fun cpu -> cpu.B)), "LD B,B", 4))
                             (0x0Euy, (Byte(ByteLoads.LD_C), "LD C,n", 8))
                             (0x4Fuy, (Register(ByteLoads.LD_C, (fun cpu -> cpu.A)), "LD C,A", 4))
                             (0x16uy, (Byte(ByteLoads.LD_D), "LD D,n", 8))
                             (0x57uy, (Register(ByteLoads.LD_D, (fun cpu -> cpu.A)), "LD D,A", 4))
                             (0x50uy, (Register(ByteLoads.LD_D, (fun cpu -> cpu.B)), "LD D,B", 4))
                             (0x54uy, (Register(ByteLoads.LD_D, (fun cpu -> cpu.H)), "LD D,H", 4))
                             (0x1Euy, (Byte(ByteLoads.LD_E), "LD E,n", 8))
                             (0x5Fuy, (Register(ByteLoads.LD_E, (fun cpu -> cpu.A)), "LD E,A", 4))
                             (0x5Duy, (Register(ByteLoads.LD_E, (fun cpu -> cpu.L)), "LD E,L", 4))
                             (0x67uy, (Register(ByteLoads.LD_H, (fun cpu -> cpu.A)), "LD H,A", 4))
                             (0x6Fuy, (Register(ByteLoads.LD_L, (fun cpu -> cpu.A)), "LD L,A", 4))
                             (0x26uy, (Byte(ByteLoads.LD_H), "LD H,n", 8))
                             (0x60uy, (Register(ByteLoads.LD_H, (fun cpu -> cpu.B)), "LD H,B", 4))
                             (0x62uy, (Register(ByteLoads.LD_H, (fun cpu -> cpu.D)), "LD H,D", 4))
                             (0x69uy, (Register(ByteLoads.LD_L, (fun cpu -> cpu.C)), "LD L,C", 4))
                             (0x6Buy, (Register(ByteLoads.LD_L, (fun cpu -> cpu.E)), "LD L,E", 4))
                             (0x2Euy, (Byte(ByteLoads.LD_L), "LD L,n", 8))
                             (0xE2uy, (Void(ByteLoads.LD_FF00_C_A), "LD (C),A", 8))
                             (0x7Euy, (Void(ByteLoads.LD_A_HL), "LD A,(HL)", 8))
                             (0x46uy, (Void(ByteLoads.LD_B_HL), "LD B,(HL)", 8))
                             (0x4Euy, (Void(ByteLoads.LD_C_HL), "LD C,(HL)", 8))
                             (0x56uy, (Void(ByteLoads.LD_D_HL), "LD D,(HL)", 8))
                             (0x5Euy, (Void(ByteLoads.LD_E_HL), "LD E,(HL)", 8))
                             (0x66uy, (Void(ByteLoads.LD_H_HL), "LD H,(HL)", 8))
                             (0x3Auy, (Void(ByteLoads.LDD_A_HL), "LDD A,(HL)", 8))
                             (0x32uy, (Void(ByteLoads.LDD_HL_A), "LDD (HL),A", 8))
                             (0x2Auy, (Void(ByteLoads.LDI_A_HL), "LDI A,(HL)", 8))
                             (0x22uy, (Void(ByteLoads.LDI_HL_A), "LDI (HL),A", 8))
                             (0xE0uy, (Byte(ByteLoads.LD_n_A), "LDH (n),A", 12))
                             (0xF0uy, (Byte(ByteLoads.LD_A_n), "LDH A,(n)", 12))
                             (0x12uy, (Void(ByteLoads.LD_DE_A), "LD (DE),A", 8))
                             (0x71uy, (Register(ByteLoads.LD_HL_n, (fun cpu -> cpu.C)), "LD (HL),C", 8))
                             (0x72uy, (Register(ByteLoads.LD_HL_n, (fun cpu -> cpu.D)), "LD (HL),D", 8))
                             (0x73uy, (Register(ByteLoads.LD_HL_n, (fun cpu -> cpu.E)), "LD (HL),E", 8))
                             (0x36uy, (Byte(ByteLoads.LD_HL_n), "LD (HL),n", 12))
                             (0x77uy, (Void(ByteLoads.LD_HL_A), "LD (HL),A", 8))
                             (0xEAuy, (Short(ByteLoads.LD_nn_A), "LD (nn),A", 16))

                             (0x3Cuy, (Void(ByteALU.INC_A), "INC A", 4))
                             (0x0Cuy, (Void(ByteALU.INC_C), "INC C", 4))
                             (0x14uy, (Void(ByteALU.INC_D), "INC D", 4))
                             (0x1Cuy, (Void(ByteALU.INC_E), "INC E", 4))
                             (0x24uy, (Void(ByteALU.INC_H), "INC H", 4))
                             (0x2Cuy, (Void(ByteALU.INC_L), "INC L", 4))
                             (0x34uy, (Void(ByteALU.INC_addr_HL), "INC (HL)", 12))
                             (0x3Duy, (Void(ByteALU.DEC_A), "DEC A", 4))
                             (0x05uy, (Void(ByteALU.DEC_B), "DEC B", 4))
                             (0x0Duy, (Void(ByteALU.DEC_C), "DEC C", 4))
                             (0x1Duy, (Void(ByteALU.DEC_E), "DEC E", 4))
                             (0x2Duy, (Void(ByteALU.DEC_L), "DEC L", 4))
                             (0x35uy, (Void(ByteALU.DEC_addr_HL), "DEC (HL)", 12))
                             (0x87uy, (Register(ByteALU.ADD_n, (fun cpu -> cpu.A)), "ADD A,A", 4))
                             (0x85uy, (Register(ByteALU.ADD_n, (fun cpu -> cpu.L)), "ADD A,L", 4))
                             (0xC6uy, (Byte(ByteALU.ADD_n), "ADD A,n", 8))
                             (0x86uy, (Void(ByteALU.ADD_HL), "ADD (HL)", 8))
                             (0xCEuy, (Byte(ByteALU.ADC), "ADC A,n", 8))
                             (0x8Euy, (Void(ByteALU.ADC_HL), "ADC HL", 8))
                             (0xA7uy, (Register(ByteALU.AND, (fun cpu -> cpu.A)), "AND A", 8))
                             (0xA1uy, (Register(ByteALU.AND, (fun cpu -> cpu.C)), "AND C", 8))
                             (0xE6uy, (Byte(ByteALU.AND), "AND #", 8))
                             (0xB0uy, (Register(ByteALU.OR, (fun cpu -> cpu.B)), "OR B", 4))
                             (0xB1uy, (Register(ByteALU.OR, (fun cpu -> cpu.C)), "OR C", 4))
                             (0xB5uy, (Register(ByteALU.OR, (fun cpu -> cpu.L)), "OR L", 4))
                             (0xF6uy, (Byte(ByteALU.OR), "OR n", 8))
                             (0xAFuy, (Register(ByteALU.XOR, (fun cpu -> cpu.A)), "XOR A", 4))
                             (0xA9uy, (Register(ByteALU.XOR, (fun cpu -> cpu.C)), "XOR C", 4))
                             (0xEEuy, (Byte(ByteALU.XOR), "XOR n", 8))
                             (0xFEuy, (Byte(ByteALU.CP_n), "CP #", 8))

                             (0x0uy, (Const(Control.NOP), "NOP", 4))

                             (0xC3uy, (Short(Jump.JP), "JP NN", 16))
                             (0xC2uy, (Short(Jump.JP_NZ), "JP_NZ NN", 12))
                             (0xCAuy, (Short(Jump.JP_Z), "JP_Z NN", 12))
                             (0xE9uy, (Void(Jump.JP_HL), "JP (HL)", 4))
                             (0x18uy, (Byte(Jump.JR), "JR", 8))
                             (0x20uy, (Byte(Jump.JR_NZ), "JR NZ", 8))
                             (0x28uy, (Byte(Jump.JR_Z), "JR N", 8))
                             (0x30uy, (Byte(Jump.JR_NC), "JR NC", 8))
                             (0x38uy, (Byte(Jump.JR_C), "JR C", 8))
                             (0xEFuy, (VoidExtra(Jump.RST, 0x28), "RST 28H", 16))
                             (0xFFuy, (VoidExtra(Jump.RST, 0x38), "RST 38H", 16))

                             (0xCDuy, (Short(Calls.CALL), "CALL nn", 12))
                             (0xCCuy, (Short(Calls.CALL_Z), "CALL Z,nn", 12))

                             (0xC9uy, (Void(Returns.RET), "RET", 8))
                             (0xC0uy, (Void(Returns.RET_NZ), "RET NZ", 8))
                             (0xC8uy, (Void(Returns.RET_Z), "RET Z", 8))
                             (0xD0uy, (Void(Returns.RET_NC), "RET NC", 8))
                             (0xD8uy, (Void(Returns.RET_C), "RET C", 8))
                             (0xD9uy, (Void(Returns.RETI), "RETI", 8))

                             (0x01uy, (Short(ShortLoads.LD_BC), "LD BC,nn", 12))
                             (0x11uy, (Short(ShortLoads.LD_DE), "LD DE,nn", 12))
                             (0x21uy, (Short(ShortLoads.LD_HL), "LD HL,nn", 12))
                             (0x31uy, (Short(ShortLoads.LD_SP), "LD SP,nn", 12))
                             (0x08uy, (Short(ShortLoads.LD_nn_SP), "LD (nn),SP", 20))
                             (0xF5uy, (Void(ShortLoads.PUSH_AF), "PUSH AF", 16))
                             (0xC5uy, (Void(ShortLoads.PUSH_BC), "PUSH BC", 16))
                             (0xD5uy, (Void(ShortLoads.PUSH_DE), "PUSH DE",  16))
                             (0xE5uy, (Void(ShortLoads.PUSH_HL), "PUSH HL", 16))
                             (0xF1uy, (Void(ShortLoads.POP_AF), "POP AF",   12))
                             (0xC1uy, (Void(ShortLoads.POP_BC), "POP BC",   12))
                             (0xD1uy, (Void(ShortLoads.POP_DE), "POP DE",   12))
                             (0xE1uy, (Void(ShortLoads.POP_HL), "POP HL",   12))

                             (0x09uy, (Void(ShortALU.ADD_HL_BC), "ADD HL,BC", 8))
                             (0x19uy, (Void(ShortALU.ADD_HL_DE), "ADD HL,DE", 8))
                             (0x39uy, (Void(ShortALU.ADD_HL_SP), "ADD HL,SP", 8))
                             (0x03uy, (Void(ShortALU.INC_BC), "INC BC", 8))
                             (0x13uy, (Void(ShortALU.INC_DE), "INC DE", 8))
                             (0x23uy, (Void(ShortALU.INC_HL), "INC HL", 8))
                             (0x33uy, (Void(ShortALU.INC_SP), "INC SP", 8))
                             (0x0Buy, (Void(ShortALU.DEC_BC), "DEC BC", 8))
                             (0x1Buy, (Void(ShortALU.DEC_DE), "DEC DE", 8))
                             (0x2Buy, (Void(ShortALU.DEC_HL), "DEC HL", 8))

                             (0x37uy, (Void(Misc.SCF), "SCF", 4))
                             (0x27uy, (Void(Misc.DAA), "DAA", 4))
                             (0x2Fuy, (Void(Misc.CPL), "CPL", 4))
                             (0x10uy, (Void(Misc.STOP), "STOP", 4))
                             (0xF3uy, (Void(Misc.DI), "DI", 4))
                             (0xFBuy, (Void(Misc.IE), "IE", 4))

                             (0x07uy, (Void(RotatesShifts.RLCA), "RLCA", 4))
                             (0x1Fuy, (Void(RotatesShifts.RRA), "RRA", 4)) ]

    let internal CB_EXTENSIONS = 
        SBInstructionTable [ (0x87uy, (Void(Bit.RES_0_A), "RES 0,A", 8))
                             (0x86uy, (Void(Bit.RES_0_HL), "RES 0,(HL)", 8))
                             (0x27uy, (Void(RotatesShifts.SLA_A), "SLA A", 8))
                             (0x37uy, (Void(Misc.SWAP_A), "SWAP A", 8))
                             (0x50uy, (Void(Bit.BIT_2_B), "BIT 2,B", 8))
                             (0x58uy, (Void(Bit.BIT_3_B), "BIT 3,B", 8))
                             (0x60uy, (Void(Bit.BIT_4_B), "BIT 4,B", 8))
                             (0x68uy, (Void(Bit.BIT_5_B), "BIT 5,B", 8))
                             (0x7Euy, (Void(Bit.BIT_7_HL), "BIT 7,(HL)", 8))
                             (0x7Fuy, (Void(Bit.BIT_7_A), "BIT 7,A", 8))]
