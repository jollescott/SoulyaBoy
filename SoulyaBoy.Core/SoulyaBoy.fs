namespace SoulyaBoy.Core

module SoulyaBoy = 
    let CreateSoulyaBoy rom = 
        SBFactory.CreateSB(rom)

    let Run(sb) = 
        let rec loop sb cycle =         
            match SBExecutor.Execute sb cycle with
            | Some (_, mutated) -> loop mutated (cycle + 1)
            | None -> ()

        loop sb 0