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

    private static IWindow? _window;
    private static SBMb? _sbmb;
    private static Renderer? _renderer;

    private static double _lastUpdatedTime;

    private static void Update(double deltaTime)
    {
        if(_lastUpdatedTime < 1 / CPU_FREQ)
        {
            _lastUpdatedTime += deltaTime;
            return;
        } else
        {
            _lastUpdatedTime = 0;
        }

        try
        {
            // TODO: Redo and replace catch expression.
            _sbmb = Core.SoulyaBoy.Run(_sbmb, _renderer).Value;
        } catch (NullReferenceException)
        {
            Debug.WriteLine("[Panic] Emulator panicked.");
            _window?.Close();
        }
    }

    private static void Render(double deltaTime)
    {
        _renderer?.Render(deltaTime);
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
        _window.Update += Update;

        _window.Run();
    }
}