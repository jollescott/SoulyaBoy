using Silk.NET.OpenGL;
using Silk.NET.Windowing;
using System.Drawing;

namespace SoulyaBoy.Desktop
{
    internal class Renderer : Core.SoulyaBoy.IPixelPipe
    {
        private GL _gl;

        public Renderer(IWindow? window)
        {
            _gl = window.CreateOpenGL();
        }

        public void Render(double deltaTime)
        {
            _gl.ClearColor(Color.CornflowerBlue);
            _gl.Clear(ClearBufferMask.ColorBufferBit);
        } 

        public void Execute(byte value)
        {
            
        }
    }
}
