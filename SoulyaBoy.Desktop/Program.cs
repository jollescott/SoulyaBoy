using System.Diagnostics;
using Silk.NET.Input;
using Silk.NET.Maths;
using Silk.NET.Windowing;
using SoulyaBoy.Core;

namespace SoulyaBoy.Desktop;

public static class Program
{
    private static Thread? _emulatorThread;
    private static IWindow? _window;
    private static Renderer? _renderer;

    private static SBMb _sbmb;
    private static SBInput _input = SBInput.None;

    private static bool _running = true;

    private static readonly Stopwatch ExecutionStopwatch = new();
    private static int _executionTimesIndex;
    private const int ExecutionTimeSamples = 1000;
    private static readonly long[] ExecutionTimes = new long[ExecutionTimeSamples];

    private static double _cpuFrequencyTimer;
    
    private static void EmulatorProc()
    {
        while (_running)
        {
            ExecutionStopwatch.Restart();
            _sbmb = Core.SoulyaBoy.Run(_sbmb, _input, _renderer);
            var val = ExecutionStopwatch.ElapsedTicks;
            ExecutionTimes[_executionTimesIndex] = val;
            _executionTimesIndex = _executionTimesIndex >= ExecutionTimeSamples - 1 ? 0 : _executionTimesIndex + 1;
        }
    }

    private static void OnRender(double deltaTime)
    {
        _renderer?.Render();
    }

    private static void OnUpdate(double deltaTime)
    {
        _cpuFrequencyTimer += deltaTime;
        
        if (!(_cpuFrequencyTimer > 1)) return;
        _cpuFrequencyTimer = 0;
        var cycleTime = ExecutionTimes.Average() / Stopwatch.Frequency;
        
        if (_window == null || cycleTime == 0) return;
        
        var cycleFrequency = 1 / cycleTime;
        _window.Title = $"SoulyaBoy ({Math.Round(cycleFrequency/Math.Pow(10,6),4)} MHz)";
    }

    private static void OnLoad()
    {
        _renderer = new(_window);

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

        _emulatorThread = new(EmulatorProc);
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

    public static void Main()
    {
        var options = WindowOptions.Default with
        {
            Size = new Vector2D<int>(800, 600),
            Title = "SoulyaBoy"
        };

        _window = Window.Create(options);

        _window.Load += OnLoad;
        _window.Render += OnRender;
        _window.Update += OnUpdate;
        _window.Closing += OnClosing;

        _window.Run();
        _window.Dispose();
    }
}