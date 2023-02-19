open System.IO

open SoulyaBoy.Core

let rom = File.ReadAllBytes("tetris.gb")
let sb = SoulyaBoy.CreateSoulyaBoy(rom)

SoulyaBoy.Run(sb)
