namespace SoulyaBoy.Core

type SBResult<'a> = 'a * SBMb

type SB<'a> = 
    SB of (SBMb -> Result<SBResult<'a>, string>)

module SB =
    
    let Run (SB f) mb = f mb

    let Return x = SB(fun mb -> Ok(x, mb))

    let Panic x = SB(fun _ -> Error(x))

    let Map f prev = 
        let doMap mb =
            let prevResult = Run prev mb

            match prevResult with
            | Ok(r, mmb) -> (f r) mmb
            | Error(e) -> Error(e)

        SB doMap

    let Bind f prev = 
        let doBind mb = 
            let prevResult = Run prev mb
            
            match prevResult with
            | Ok(r, mmb) -> Run (f r) mmb
            | Error(e) -> Error(e)

        SB doBind

    let Get = SB(fun mb -> Ok(mb, mb))

    let Put mmb = SB(fun _ -> Ok((), mmb))

type SBBuilder() =
    member _.Zero() = SB.Return ()
    member _.Return(x) = SB.Return x
    member _.ReturnFrom(x) = x
    member _.Bind(x,f) = SB.Bind f x