namespace SoulyaBoy.Core

[<Struct>]
type SB<'a> = SB of (SBMb -> struct ('a * SBMb))

module SB =
    let inline Run (SB f) mb = f mb
    let Get = SB(fun mb -> struct (mb, mb))
    let Put mmb = SB(fun _ -> struct ((), mmb))

type SBBuilder() =
    member inline _.Zero() = SB(fun mb -> ((), mb))
    member inline _.Return(x) = SB(fun mb -> (x, mb))
    member inline _.ReturnFrom(x) = x

    member inline _.Bind(x, [<InlineIfLambda>] f) =
        let doBind mb =
            let struct (r, mmb) = SB.Run x mb
            SB.Run (f r) mmb

        SB doBind

    member this.Combine(a, b) = this.Bind(a, (fun () -> b))

    member inline _.Delay([<InlineIfLambda>] f) = f ()

    member inline this.While(cond, [<InlineIfLambda>] f) =
        SB (fun mb ->
            let mutable whileMB = mb

            while cond () do
                let struct (_, iterMB) = SB.Run (f ()) whileMB
                whileMB <- iterMB

            ((), whileMB))

    member inline this.For(m: seq<'T>, [<InlineIfLambda>] f) =
        let ie = m.GetEnumerator()

        let result = this.While((fun () -> ie.MoveNext()), (fun () -> f ie.Current))
        ie.Dispose()
        result
