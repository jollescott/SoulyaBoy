namespace SoulyaBoy.Core

module SBUtils =
    let internal toShort (high: byte) (low: byte) =
        // Important to cast else bytes will wrap with <<< shift.
        uint16 high <<< 8 ||| uint16 low


    let internal toBytes short =
        let high = byte (short >>> 8)
        let low = byte (short &&& 0b1111_1111us)
        struct (high, low)

    let internal bind f arg =
        match arg with
        | Some s -> f s
        | None -> None
