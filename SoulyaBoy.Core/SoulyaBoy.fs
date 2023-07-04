namespace SoulyaBoy.Core

module SoulyaBoy =
    let CreateSoulyaBoy rom = SBMbFactory.CreateSBMb(rom)

    let Run mb =
        let rec loop mb cycle =
            match SBExecutor.Execute mb cycle with
            | Some (cycles, mutated) -> loop mutated (cycle + cycles)
            | None -> ()

        loop mb 0
