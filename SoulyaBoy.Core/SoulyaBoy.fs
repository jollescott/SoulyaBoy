namespace SoulyaBoy.Core

module SoulyaBoy =
    type IPixelPipe =
        abstract DrawPixel : int -> int -> byte -> unit

    let CreateSoulyaBoy rom = SBMbFactory.CreateSBMb(rom)

    let Run mb (pixelPipe: IPixelPipe) =
        let struct (r, mb) = SB.Run (SBExecutor.Run pixelPipe.DrawPixel) mb
        mb
