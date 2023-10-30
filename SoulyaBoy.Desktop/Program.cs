using Silk.NET.Windowing;
using SoulyaBoy.Core;
using System.Diagnostics;

namespace SoulyaBoy.Desktop;

public class Program
{
    /// <summary>
    /// The CPU frequency (Hz) of the original Gameboy.
    /// </summary>
    private static readonly double CPU_FREQ = 4194304;
    private static Thread? _emulatorThread;
    private static IWindow? _window;
    private static SBMb? _sbmb;
    private static Renderer? _renderer;

    private static bool _running = true;

    private static void EmulatorProc()
    {
        while (_running)
        {
            if(_sbmb != null)
            {
                _sbmb = Core.SoulyaBoy.Run(_sbmb, _renderer).Value;
            }
        }
    }

    private static void Render(double deltaTime)
    {
        _renderer?.Render(deltaTime);
        _window?.SwapBuffers();
    }

    private static void Load()
    {
        _renderer = new Renderer(_window);

        try
        {
            var rom = File.ReadAllBytes("tetris.gb");
            _sbmb = Core.SoulyaBoy.CreateSoulyaBoy(rom);
        }
        catch (IOException)
        {
            Debug.WriteLine("[Program] could not find rom.");
            _window?.Close();
        }

        _emulatorThread = new Thread(new ThreadStart(EmulatorProc));
        _emulatorThread.Start();
    }

    private static void Close()
    {
        _running = false;
        _emulatorThread?.Join();

        _renderer?.Close();
    }

    public static void Main(string[] args)
    {
        var options = WindowOptions.Default with
        {
            Size = new Silk.NET.Maths.Vector2D<int>(800, 600),
            Title = "SoulyaBoy"
        };

        _window = Window.Create(options);

        _window.Load += Load;
        _window.Render += Render;
        _window.Closing += Close;

        _window.Run();
        _window.Dispose();
    }
}