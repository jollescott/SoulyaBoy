namespace SoulyaBoy.Core

module SoulyaBoy = 
    let CreateSoulyaBoy rom = 
        SBFactory.CreateSB(rom)

    let Run(sb) = 
        let rec loop sb =         
            match SBExecutor.Execute sb with
            | Some (_, mutated) -> loop mutated 
            | None -> ()

        loop(sb)