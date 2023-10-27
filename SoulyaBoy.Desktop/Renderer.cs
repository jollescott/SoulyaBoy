using Silk.NET.OpenGL;
using Silk.NET.Windowing;
using System.Drawing;

namespace SoulyaBoy.Desktop
{
    internal class Renderer : Core.SoulyaBoy.IPixelPipe
    {
        private readonly GL _gl;

        private readonly uint _vao;
        private readonly uint _vbo;
        private readonly uint _ebo;
        private readonly uint _shaderProgram;

        private readonly float[] QUAD_VERTICES =
        {
             1.0f,  1.0f, 0.0f,
             1.0f, -1.0f, 0.0f,
            -1.0f, -1.0f, 0.0f,
            -1.0f,  1.0f, 0.0f
        };

        private readonly uint[] INDICES =
        {
            0u, 1u, 3u,
            1u, 2u, 3u
        };

        private const string VERTEX_SHADER_SOURCE = @"
        #version 330 core

        layout (location = 0) in vec3 aPosition;

        void main()
        {
            gl_Position = vec4(aPosition, 1.0);
        }";

        private const string FRAGMENT_SHADER_SOURCE = @"
        #version 330 core

        out vec4 out_color;

        void main()
        {
            out_color = vec4(1.0, 0.5, 0.2, 1.0);
        }";

        public unsafe Renderer(IWindow? window)
        {
            _gl = window.CreateOpenGL();

            _vao = _gl.GenVertexArray();
            _gl.BindVertexArray(_vao);

            #region Buffers
            _vbo = _gl.GenBuffer();
            _gl.BindBuffer(BufferTargetARB.ArrayBuffer, _vbo);

            fixed (float* buf = QUAD_VERTICES)
            {
                _gl.BufferData(BufferTargetARB.ArrayBuffer, (nuint)(QUAD_VERTICES.Length * sizeof(float)), buf, BufferUsageARB.StaticDraw);
            }

            _ebo = _gl.GenBuffer();
            _gl.BindBuffer(BufferTargetARB.ElementArrayBuffer, _ebo);

            fixed (uint* buf = INDICES)
            {
                _gl.BufferData(BufferTargetARB.ElementArrayBuffer, (nuint)(INDICES.Length * sizeof(uint)), buf, BufferUsageARB.StaticDraw);
            }

            #endregion

            #region Shaders 

            var vertexShader = _gl.CreateShader(ShaderType.VertexShader);
            _gl.ShaderSource(vertexShader, VERTEX_SHADER_SOURCE);
            _gl.CompileShader(vertexShader);

            _gl.GetShader(vertexShader, GLEnum.CompileStatus, out int vertexStatus);

            if(vertexStatus != (int) GLEnum.True)
            {
                throw new Exception("Failed to compile vertex shader: " + _gl.GetShaderInfoLog(vertexShader));
            }

            var fragmentShader = _gl.CreateShader(ShaderType.FragmentShader);
            _gl.ShaderSource(fragmentShader, FRAGMENT_SHADER_SOURCE);
            _gl.CompileShader(fragmentShader);

            _gl.GetShader(fragmentShader, GLEnum.CompileStatus, out int  fragmentStatus);

            if(fragmentStatus != (int) GLEnum.True)
            {
                throw new Exception("Failed to compile fragment shader:" + _gl.GetShaderInfoLog(fragmentShader));
            }

            _shaderProgram = _gl.CreateProgram();
            _gl.AttachShader(_shaderProgram, vertexShader);
            _gl.AttachShader(_shaderProgram, fragmentShader);
            _gl.LinkProgram(_shaderProgram);

            _gl.GetProgram(_shaderProgram, GLEnum.LinkStatus, out int programStatus);

            if(programStatus != (int) GLEnum.True)
            {
                throw new Exception("Failed to link shader program: " + _gl.GetProgramInfoLog(_shaderProgram));
            }

            _gl.DetachShader(_shaderProgram, vertexShader);
            _gl.DetachShader(_shaderProgram, fragmentShader);
            _gl.DeleteShader(vertexShader);
            _gl.DeleteShader(fragmentShader);

            #endregion

            const uint positionLoc = 0;
            _gl.EnableVertexAttribArray(positionLoc);
            _gl.VertexAttribPointer(positionLoc, 3, VertexAttribPointerType.Float, false, 3 * sizeof(float), (void*)0);

            #region Cleanup
            _gl.BindVertexArray(0);
            _gl.BindBuffer(BufferTargetARB.ArrayBuffer, 0);
            _gl.BindBuffer(BufferTargetARB.ElementArrayBuffer, 0);
            #endregion
        }

        public unsafe void Render(double deltaTime)
        {
            _gl.ClearColor(Color.CornflowerBlue);
            _gl.Clear(ClearBufferMask.ColorBufferBit);

            _gl.BindVertexArray(_vao);
            _gl.UseProgram(_shaderProgram);
            _gl.DrawElements(PrimitiveType.Triangles, 6, DrawElementsType.UnsignedInt, (void*)0);
        } 


        public void DrawPixel(int px, int py, byte shade)
        {
            
        }

        internal void Close()
        {
            _gl.DeleteBuffer(_vbo);
            _gl.DeleteBuffer(_ebo);
            _gl.DeleteVertexArray(_vao);
        }
    }
}
