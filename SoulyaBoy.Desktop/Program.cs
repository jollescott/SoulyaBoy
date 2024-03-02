using Silk.NET.Windowing;
using SoulyaBoy.Core;
using System.Diagnostics;
using Silk.NET.Input;

namespace SoulyaBoy.Desktop;

public class Program
{
    /// <summary>
    /// The CPU frequency (Hz) of the original Gameboy.
    /// </summary>
    private static readonly double CPU_FREQ = 4194304;
    private static Thread? _emulatorThread;
    private static IWindow? _window;
    private static Renderer? _renderer;

    private static SBMb _sbmb;
    private static SBInput _input = SBInput.None;
    
    private static bool _running = true;

    private static void EmulatorProc()
    {
        while (_running)
        {
            _sbmb = Core.SoulyaBoy.Run(_sbmb, _input, _renderer);
        }
    }

    private static void OnRender(double deltaTime)
    {
        _renderer?.Render(deltaTime);
    }

    private static void OnLoad()
    {
        _renderer = new Renderer(_window);

        var inputContext = _window!.CreateInput();
        inputContext.Keyboards[0].KeyDown += OnKeyDown;
        inputContext.Keyboards[0].KeyUp += OnKeyUp;

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

        _emulatorThread = new Thread(EmulatorProc);
        _emulatorThread.Start();
    }

    private static void OnKeyUp(IKeyboard keyboard, Key key, int keycode)
    {
        switch (key)
        {
            case Key.W:
                _input &= ~SBInput.Up;
                break;
            case Key.S:
                _input &= ~SBInput.Down;
                break;
            case Key.A:
                _input &= ~SBInput.Left;
                break;
            case Key.D:
                _input &= ~SBInput.Right;
                break;
            case Key.H:
                _input &= ~SBInput.Start;
                break;
            case Key.J:
                _input &= ~SBInput.Select;
                break;
            case Key.K:
                _input &= ~SBInput.A;
                break;
            case Key.L:
                _input &= ~SBInput.B;
                break;
        }
    }

    private static void OnKeyDown(IKeyboard keyboard, Key key, int keycode)
    {
        switch (key)
        {
            case Key.W:
                _input |= SBInput.Up;
                break;
            case Key.S:
                _input |= SBInput.Down;
                break;
            case Key.A:
                _input |= SBInput.Left;
                break;
            case Key.D:
                _input |= SBInput.Right;
                break;
            case Key.H:
                _input |= SBInput.Start;
                break;
            case Key.J:
                _input |= SBInput.Select;
                break;
            case Key.K:
                _input |= SBInput.A;
                break;
            case Key.L:
                _input |= SBInput.B;
                break;
        }
    }

    private static void OnClosing()
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
            Title = "SoulyaBoy",
        };

        _window = Window.Create(options);

        _window.Load += OnLoad;
        _window.Render += OnRender;
        _window.Closing += OnClosing;

        _window.Run();
        _window.Dispose();
    }
}