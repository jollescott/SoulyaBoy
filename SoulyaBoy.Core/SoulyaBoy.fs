namespace SoulyaBoy.Core

module SoulyaBoy = 
    let CreateSoulyaBoy rom = 
        SBFactory.CreateSB(rom)

    let Run(sb) = 
        while true do 
            SBOpcodes.Execute sb
            
            ()