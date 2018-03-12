// Copyright 2015 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#pragma once

#include <utility>
#include <glad/glad.h>
#include "common/common_types.h"
#include "video_core/renderer_opengl/gl_shader_util.h"
#include "video_core/renderer_opengl/gl_state.h"

class OGLTexture : private NonCopyable {
public:
    OGLTexture() = default;
    OGLTexture(OGLTexture&& o) {
        std::swap(handle, o.handle);
    }
    ~OGLTexture() {
        Release();
    }
    OGLTexture& operator=(OGLTexture&& o) {
        std::swap(handle, o.handle);
        return *this;
    }

    /// Creates a new internal OpenGL resource and stores the handle
    void Create() {
        if (handle != 0)
            return;
        glGenTextures(1, &handle);
    }

    /// Deletes the internal OpenGL resource
    void Release() {
        if (handle == 0)
            return;
        glDeleteTextures(1, &handle);
        OpenGLState::GetCurState().ResetTexture(handle).Apply();
        handle = 0;
    }

    GLuint handle = 0;
};

class OGLSampler : private NonCopyable {
public:
    OGLSampler() = default;
    OGLSampler(OGLSampler&& o) {
        std::swap(handle, o.handle);
    }
    ~OGLSampler() {
        Release();
    }
    OGLSampler& operator=(OGLSampler&& o) {
        std::swap(handle, o.handle);
        return *this;
    }

    /// Creates a new internal OpenGL resource and stores the handle
    void Create() {
        if (handle != 0)
            return;
        glGenSamplers(1, &handle);
    }

    /// Deletes the internal OpenGL resource
    void Release() {
        if (handle == 0)
            return;
        glDeleteSamplers(1, &handle);
        OpenGLState::GetCurState().ResetSampler(handle).Apply();
        handle = 0;
    }

    GLuint handle = 0;
};

class OGLShader : private NonCopyable {
public:
    OGLShader() = default;
    OGLShader(OGLShader&& o) {
        std::swap(handle, o.handle);
    }
    ~OGLShader() {
        Release();
    }
    OGLShader& operator=(OGLShader&& o) {
        std::swap(handle, o.handle);
        return *this;
    }

    /// Creates a new internal OpenGL resource and stores the handle
    void Create(const char* vert_shader, const char* frag_shader) {
        if (handle != 0)
            return;
        handle = GLShader::LoadProgram(vert_shader, frag_shader);
    }

    /// Deletes the internal OpenGL resource
    void Release() {
        if (handle == 0)
            return;
        glDeleteProgram(handle);
        OpenGLState::GetCurState().ResetProgram(handle).Apply();
        handle = 0;
    }

    GLuint handle = 0;
};

class OGLBuffer : private NonCopyable {
public:
    OGLBuffer() = default;
    OGLBuffer(OGLBuffer&& o) {
        std::swap(handle, o.handle);
    }
    ~OGLBuffer() {
        Release();
    }
    OGLBuffer& operator=(OGLBuffer&& o) {
        std::swap(handle, o.handle);
        return *this;
    }

    /// Creates a new internal OpenGL resource and stores the handle
    void Create() {
        if (handle != 0)
            return;
        glGenBuffers(1, &handle);
    }

    /// Deletes the internal OpenGL resource
    void Release() {
        if (handle == 0)
            return;
        glDeleteBuffers(1, &handle);
        OpenGLState::GetCurState().ResetBuffer(handle).Apply();
        handle = 0;
    }

    GLuint handle = 0;
};

class OGLVertexArray : private NonCopyable {
public:
    OGLVertexArray() = default;
    OGLVertexArray(OGLVertexArray&& o) {
        std::swap(handle, o.handle);
    }
    ~OGLVertexArray() {
        Release();
    }
    OGLVertexArray& operator=(OGLVertexArray&& o) {
        std::swap(handle, o.handle);
        return *this;
    }

    /// Creates a new internal OpenGL resource and stores the handle
    void Create() {
        if (handle != 0)
            return;
        glGenVertexArrays(1, &handle);
    }

    /// Deletes the internal OpenGL resource
    void Release() {
        if (handle == 0)
            return;
        glDeleteVertexArrays(1, &handle);
        OpenGLState::GetCurState().ResetVertexArray(handle).Apply();
        handle = 0;
    }

    GLuint handle = 0;
};

class OGLFramebuffer : private NonCopyable {
public:
    OGLFramebuffer() = default;
    OGLFramebuffer(OGLFramebuffer&& o) {
        std::swap(handle, o.handle);
    }
    ~OGLFramebuffer() {
        Release();
    }
    OGLFramebuffer& operator=(OGLFramebuffer&& o) {
        std::swap(handle, o.handle);
        return *this;
    }

    /// Creates a new internal OpenGL resource and stores the handle
    void Create() {
        if (handle != 0)
            return;
        glGenFramebuffers(1, &handle);
    }

    /// Deletes the internal OpenGL resource
    void Release() {
        if (handle == 0)
            return;
        glDeleteFramebuffers(1, &handle);
        OpenGLState::GetCurState().ResetFramebuffer(handle).Apply();
        handle = 0;
    }

    GLuint handle = 0;
};
