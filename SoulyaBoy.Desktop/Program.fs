open System.IO

open SoulyaBoy.Core

let rom = File.ReadAllBytes("tetris.gb")
let mb = SoulyaBoy.CreateSoulyaBoy(rom)

SoulyaBoy.Run(mb)
