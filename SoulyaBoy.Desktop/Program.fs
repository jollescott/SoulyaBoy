open System.IO

open SoulyaBoy.Core

let rom = File.ReadAllBytes("tetris.gb")
let sb = SoulyaBoy.CreateSB(rom)