namespace SoulyaBoy.Core

type SBResult<'a> = 'a * SBMb

type SB<'a> = 
    SB of (SBMb -> Result<SBResult<'a>, string>)

module SB =
    
    let Run (SB f) mb = f mb

    let Return x = SB(fun mb -> Ok(x, mb))

    let Panic x = SB(fun _ -> Error(x))

    let Bind f prev = 
        let doBind mb = 
            let prevResult = Run prev mb
            
            match prevResult with
            | Ok(r, mmb) -> Run (f r) mmb
            | Error(e) -> Error(e)

        SB doBind

    let Combine a _ = 
        a

    let Delay f = Bind f (Return ())

    let rec internal While cond f = 
        if cond() then f() |> Bind (fun _ -> While cond f)
        else Return ()

    let For (m: seq<'T>) f = 
        let ie = m.GetEnumerator();

        let result = While (fun () -> ie.MoveNext()) (fun () -> f ie.Current)
        ie.Dispose();
        result

    let Get = SB(fun mb -> Ok(mb, mb))

    let Put mmb = SB(fun _ -> Ok((), mmb))

type SBBuilder() =
    member _.Zero() = SB.Return ()
    member _.Return(x) = SB.Return x
    member _.ReturnFrom(x) = x
    member _.Bind(x,f) = SB.Bind f x
    member _.Combine(a,b) = SB.Combine a b
    member _.Delay(f) = SB.Delay f
    member _.For(m,f) = SB.For m f