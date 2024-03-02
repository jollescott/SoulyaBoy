namespace SoulyaBoy.Core

module SoulyaBoy =
    type IPixelPipe =
        abstract DrawPixel : int -> int -> byte -> unit

    let CreateSoulyaBoy rom = SBMbFactory.CreateSBMb(rom)

    let Run oldMb (input: SBInput) (pixelPipe: IPixelPipe) =        
        let oldJoypad = oldMb.Joypad 
        
        let joypad = if (oldJoypad &&& 0b11_0000uy) <> 0uy then
                        let buttons = match oldJoypad &&& 0b11_0000uy with
                                        | 0b01_0000uy -> byte (~~~input) &&& 0xFuy
                                        | 0b10_0000uy -> byte (~~~input >>> 4) &&& 0xFuy
                                        | 0b11_0000uy -> 0b1111uy
                                        | _ -> 0uy
                        (oldJoypad &&& 0b11_0000uy) ||| buttons
                        else oldJoypad
                
        let struct (r, mb) = SB.Run (SBExecutor.Run pixelPipe.DrawPixel) { oldMb with Joypad = joypad }
        mb
