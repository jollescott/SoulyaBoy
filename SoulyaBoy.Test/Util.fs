namespace SoulyaBoy.Test

open SoulyaBoy.Core

module Util =
    let private ROM_SIZE = 16000
    let createTestSB = SBFactory.CreateSB(Array.zeroCreate ROM_SIZE)
