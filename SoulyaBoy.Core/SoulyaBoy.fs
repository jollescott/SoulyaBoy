namespace SoulyaBoy.Core

module SoulyaBoy =
    let CreateSoulyaBoy rom = SBMbFactory.CreateSBMb(rom)

    let Run mb =
        let rec loop mb =
            let result = SB.Run SBExecutor.Execute mb

            match result with 
            | Ok(r, mmb) -> loop mmb 
            | Error(e) -> printf $"{e}"

        loop mb 
