namespace SoulyaBoy.Core

module SBInterrupts =
    type internal InterruptAddress =
        | VBlank = 0x40
        | LCDCStatus = 0x48
        | TimerOverflow = 0x50
        | SerialTransferCompletion = 0x50
        | HighToLow = 0x60
        
    