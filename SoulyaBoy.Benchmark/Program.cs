using System.Diagnostics.CodeAnalysis;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Diagnosers;
using BenchmarkDotNet.Diagnostics.Windows.Configs;
using BenchmarkDotNet.Running;
using SoulyaBoy.Core;

namespace SoulyaBoy.Benchmark
{
    [SuppressMessage("Performance", "CA1822:Mark members as static")]
    [MemoryDiagnoser]
    [JitStatsDiagnoser]
    [InliningDiagnoser(true, true)]
    [HardwareCounters(HardwareCounter.CacheMisses)]
    public class General
    {
        [Benchmark]
        public void NopInstruction()
        {
            var rom = new byte[16000];
            const SBInput input = SBInput.None;
            var pixelPipe = new DummyPixelPipe();

            var sbmb = Core.SoulyaBoy.CreateSoulyaBoy(rom);
            Core.SoulyaBoy.Run(sbmb, input, pixelPipe);
        }
    }

    public class Program
    {
        public static void Main(string[] args)
        {
            BenchmarkRunner.Run<General>();
        }
    }
}