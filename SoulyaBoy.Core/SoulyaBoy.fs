namespace SoulyaBoy.Core

module SoulyaBoy =
    type IPixelPipe =
        abstract DrawPixel : int -> int -> byte -> unit

    let CreateSoulyaBoy rom = SBMbFactory.CreateSBMb(rom)

    let Run mb (pixelPipe: IPixelPipe) =
        let result = SB.Run (SBExecutor.Run pixelPipe.DrawPixel) mb

        match result with 
        | Ok(r, mmb) -> Some mmb
        | Error(e) -> printf $"{e}"; None
