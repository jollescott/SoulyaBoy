namespace SoulyaBoy.Core

open System

[<Flags>]
type SBInput =
    | None = 0b0000_0000uy
    | A = 0b0000_0001uy
    | B = 0b0000_0010uy
    | Select = 0b0000_0100uy
    | Start = 0b0000_1000uy
    | Right = 0b0001_0000uy
    | Left = 0b0010_0000uy
    | Up = 0b0100_0000uy
    | Down = 0b1000_0000uy
