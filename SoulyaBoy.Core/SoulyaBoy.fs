namespace SoulyaBoy.Core

module SoulyaBoy =
    let CreateSoulyaBoy rom = SBFactory.CreateSB(rom)

    let Run (sb) =
        let rec loop sb cycle =
            match SBExecutor.Execute sb cycle with
            | Some (cycles, mutated) -> loop mutated (cycle + cycles)
            | None -> ()

        loop sb 0
