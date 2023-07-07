namespace SoulyaBoy.Core

type SB<'a> = 
    SB of (SBMb -> Option<'a> * SBMb)

module SB =
    
    let Run (SB f) mb = f mb

    let Return x = SB(fun mb -> x, mb)

    let Map f prev = 
        let doMap mb =
            let prevResult = Run prev mb

            match prevResult with
            | Some r, mmb -> (f r) mmb
            | None, mmb -> None, mmb

        SB doMap

    let Bind f prev = 
        let doBind mb = 
            let prevResult = Run prev mb
            
            match prevResult with
            | Some r, mmb -> Run (f r) mmb
            | None, mmb -> None, mmb

        SB doBind

    let Get = SB(fun mb -> Some mb, mb)

    let Put mmb = SB(fun _ -> Some (), mmb)

type SBBuilder() =
    member _.Zero() = SB.Return (Some ())
    member _.Return(x) = SB.Return x
    member _.ReturnFrom(x) = x
    member _.Bind(x,f) = SB.Bind f x