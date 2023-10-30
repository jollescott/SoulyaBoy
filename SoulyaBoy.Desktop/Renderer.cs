using Silk.NET.OpenGL;
using Silk.NET.Windowing;
using System.Diagnostics;
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
        private readonly uint _screenTexture;

        private readonly float[] QUAD_VERTICES =
        {
             1.0f,  1.0f, 0.0f,     1.0f, 1.0f,
             1.0f, -1.0f, 0.0f,     1.0f, 0.0f,
            -1.0f, -1.0f, 0.0f,     0.0f, 0.0f,
            -1.0f,  1.0f, 0.0f,     0.0f, 1.0f
        };

        private readonly uint[] INDICES =
        {
            0u, 1u, 3u,
            1u, 2u, 3u
        };

        private readonly uint[] SCREEN = new uint[256 * 256];

        private const string VERTEX_SHADER_SOURCE = @"
        #version 330 core

        layout (location = 0) in vec3 aPosition;
        layout (location = 1) in vec2 vertexUV;

        out vec2 uv;

        void main()
        {
            gl_Position = vec4(aPosition, 1.0);
            uv = vertexUV;
        }";

        private const string FRAGMENT_SHADER_SOURCE = @"
        #version 330 core

        in vec2 uv;

        out vec3 color;

        uniform sampler2D textureSampler;

        void main()
        {
            color = vec3(texture(textureSampler, uv).r);
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

            #region Texture

            _screenTexture = _gl.GenTexture();
            _gl.BindTexture(GLEnum.Texture2D, _screenTexture);
            _gl.TexParameter(GLEnum.Texture2D, GLEnum.TextureWrapS, (int)GLEnum.Repeat);
            _gl.TexParameter(GLEnum.Texture2D, GLEnum.TextureWrapT, (int)GLEnum.Repeat);
            _gl.TexParameter(GLEnum.Texture2D, GLEnum.TextureMinFilter, (int)GLEnum.Nearest);
            _gl.TexParameter(GLEnum.Texture2D, GLEnum.TextureMagFilter, (int)GLEnum.Linear);

            #endregion

            _gl.VertexAttribPointer(0, 3, VertexAttribPointerType.Float, false, 5 * sizeof(float), (void*)0);
            _gl.EnableVertexAttribArray(0);

            _gl.VertexAttribPointer(1, 2, VertexAttribPointerType.Float, false, 5 * sizeof(float), (void*)(3 * sizeof(float)));
            _gl.EnableVertexAttribArray(1);

            #region Cleanup
            _gl.BindVertexArray(0);
            _gl.BindBuffer(BufferTargetARB.ArrayBuffer, 0);
            _gl.BindBuffer(BufferTargetARB.ElementArrayBuffer, 0);
            _gl.BindTexture(GLEnum.Texture2D, 0);
            #endregion
        }

        public unsafe void Render(double deltaTime)
        {
            _gl.ClearColor(Color.CornflowerBlue);
            _gl.Clear(ClearBufferMask.ColorBufferBit);

            _gl.BindVertexArray(_vao);
            _gl.UseProgram(_shaderProgram);
            _gl.BindTexture(GLEnum.Texture2D, _screenTexture);

            fixed (uint* tex = SCREEN)
            {
                _gl.TexImage2D(GLEnum.Texture2D, 0, InternalFormat.Red, 256, 256, 0, GLEnum.Red, GLEnum.UnsignedByte, tex);
            }

            _gl.DrawElements(PrimitiveType.Triangles, 6, DrawElementsType.UnsignedInt, (void*)0);

            _gl.BindVertexArray(0);
            _gl.UseProgram(0);
            _gl.BindTexture(GLEnum.Texture2D, 0);
        } 


        public void DrawPixel(int px, int py, byte shade)
        {
            SCREEN[px + py * 256] = shade * 64u;
        }

        internal void Close()
        {
            _gl.DeleteTexture(_screenTexture);
            _gl.DeleteBuffer(_vbo);
            _gl.DeleteBuffer(_ebo);
            _gl.DeleteVertexArray(_vao);
        }
    }
}
