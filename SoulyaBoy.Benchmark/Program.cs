using System.Diagnostics.CodeAnalysis;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Diagnosers;
using BenchmarkDotNet.Running;
using SoulyaBoy.Core;

namespace SoulyaBoy.Benchmark
{
    [SuppressMessage("Performance", "CA1822:Mark members as static")]
    [MemoryDiagnoser]
    [HardwareCounters(
        HardwareCounter.BranchMispredictions,
        HardwareCounter.BranchInstructions)]
    public class Tetris
    {
        [Benchmark]
        public void FirstNInstructions()
        {
            var rom = File.ReadAllBytes("tetris.gb");
            const SBInput input = SBInput.None;
            var pixelPipe = new DummyPixelPipe();

            var sbmb = Core.SoulyaBoy.CreateSoulyaBoy(rom);

            for (var i = 0; i < N; i++)
            {
                sbmb = Core.SoulyaBoy.Run(sbmb, input, pixelPipe);
            }
        }

        private const int N = 10000;
    }

    public class Program
    {
        public static void Main(string[] args)
        {
            BenchmarkRunner.Run<Tetris>();
        }
    }
}