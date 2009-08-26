///////////////////////////////////////////////////////////////////////////////
// FileName:    file.c
// Created:     2009/04/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: CRT �ļ�����ʵ��
///////////////////////////////////////////////////////////////////////////////

#define _FILE_DEFINED
struct _iobuf;
typedef struct _iobuf FILE;

#include <pdl_base.h>
#include <stdio.h>
#include <malloc.h>
#include "file.h"

int __cdecl fclose(FILE* stream)
{
    PDLASSERT(NULL != stream);

    fflush(stream);
    if (!CloseHandle(stream->handle))
        return EOF;

    if (NULL != stream->buf)
        free(stream->buf);
    free(stream);
    return 0;
}

int __cdecl feof(FILE* stream)
{
    PDLASSERT(NULL != stream);
    return (stream->fileptr == stream->filesize);
}

int __cdecl ferror(FILE* stream)
{
    PDLASSERT(NULL != stream);
    if (CRT_FILE_ERROR & stream->flags)
        return 1;
    else
        return 0;
}

int __cdecl fflush(FILE* stream)
{
    DWORD cbWritten;
    BOOL bWritten = FALSE;

    PDLASSERT(NULL != stream);

    if (CRT_FILE_WRITE & stream->flags)
    {
        if (stream->bufptr > 0)
        {
            bWritten = WriteFile(stream->handle, stream->buf, stream->bufptr,
                &cbWritten, NULL);
        }
        stream->flags &= ~CRT_FILE_WRITE;
        stream->bufptr = 0;
    }

    if (bWritten)
    {
        stream->flags &= ~CRT_FILE_ERROR;
        return 0;
    }
    else
    {
        stream->flags |= CRT_FILE_ERROR;
        return EOF;
    }
}

int __cdecl fgetc(FILE* stream)
{
    char c;

    PDLASSERT(NULL != stream);

    fflush(stream);
    if (stream->fileptr == stream->filesize)
        return EOF;

    fread(&c, sizeof(char), 1, stream);
    return c;
}

char* __cdecl fgets(char* str, int n, FILE* stream)
{
    char* p = str;
    int ch;

    PDLASSERT(NULL != str && NULL != stream);
    if (0 == n)
        return NULL;

    while (n > 0)
    {
        ch = fgetc(stream);
        if (EOF == ch)
        {
            str = NULL;
            break;
        }

        *p = (char)ch;
        if ('\n' == *p)
            break;

        ++p;
        --n;
    }

    *p = '\0';
    return str;
}


FILE* __cdecl fopen(const char* filename, const char* mode)
{
    FILE* fp;
    DWORD dwDesiredAccess;
    DWORD dwShareMode;
    DWORD dwCreationDisposition;
    DWORD dwFlags = 0;
    BOOL bAppend = FALSE;
    BOOL bWhile = TRUE;

    PDLASSERT(NULL != filename && NULL != mode);

    // ���˿ո�
    while (' ' == *mode)
        ++mode;

    // ȷ����ʼȨ��
    switch (*mode)
    {
    case 'r':
        dwDesiredAccess = GENERIC_READ;
        dwShareMode = FILE_SHARE_READ;
        dwCreationDisposition = OPEN_EXISTING;
        break;
    case 'w':
        dwDesiredAccess = GENERIC_WRITE;
        dwShareMode = 0;
        dwCreationDisposition = CREATE_ALWAYS;
        break;
    case 'a':
        dwDesiredAccess = GENERIC_WRITE;
        dwShareMode = 0;
        dwCreationDisposition = OPEN_ALWAYS;
        bAppend = TRUE;
        break;
    default:
        return NULL;
    }

    // ȷ������Ȩ��
    ++mode;
    while ('\0' != *mode && bWhile)
    {
        switch (*mode)
        {
        case ' ':
            // ���˿ո�
            break;
        case '+':
            dwDesiredAccess = GENERIC_READ | GENERIC_WRITE;
            bWhile = FALSE;
            break;
        case 't':
            dwFlags |= CRT_FILE_TEXT;
            break;
        }
        ++mode;
    }

    // �����ļ�����
    fp = (FILE*)malloc(sizeof(FILE));
    fp->handle = CreateFileA(filename, dwDesiredAccess, dwShareMode, NULL,
        dwCreationDisposition, FILE_ATTRIBUTE_ARCHIVE, NULL);
    if (INVALID_HANDLE_VALUE == fp->handle)
    {
        free(fp);
        return NULL;
    }

    fp->flags = dwFlags;
    if (bAppend)
        fp->fileptr = SetFilePointer(fp->handle, 0, NULL, FILE_END);
    else
        fp->fileptr = 0;
    fp->filesize = GetFileSize(fp->handle, NULL);

    fp->buf = (PBYTE)malloc(CRT_BUF_SIZE);
    fp->bufptr = 0;
    fp->bufsize = 0;
    return fp;
}

int __cdecl fprintf(FILE* stream, const char* format, ...)
{
    va_list arglist;
    char buf[1024];
    int ret;

    PDLASSERT(NULL != stream && NULL != format);

    va_start(arglist, format);
    ret = wvsprintfA(buf, format, arglist);
    va_end(arglist);

    return fwrite(buf, sizeof(char), ret, stream);
}

int __cdecl fputs(const char* str, FILE *stream)
{
    size_t len = strlen(str);
    size_t ret = fwrite(str, sizeof(char), len, stream);
    return len == ret ? 0 : EOF;
}

size_t __cdecl fread(void* buffer, size_t size, size_t count, FILE* stream)
{
    size_t ret = 0;
    size_t cbio = 0;
    PBYTE dst = buffer;
    BOOL bRead;

    PDLASSERT(NULL != buffer && NULL != stream);

    size *= count;
    if (NULL == stream->buf)
    {
        // �޻����ȡ
        bRead = ReadFile(stream->handle, buffer, size, &ret, NULL);
        if (bRead)
            stream->flags &= ~CRT_FILE_ERROR;
        else
            stream->flags |= CRT_FILE_ERROR;
        return ret;
    }

    // ���д���־�������¼
    if (CRT_FILE_WRITE & stream->flags)
    {
        fflush(stream);
        stream->bufsize = 0;
    }

    while (size > 0)
    {
        if (stream->fileptr == stream->filesize)
            return ret;

        if (stream->bufsize == stream->bufptr)
        {
            // �������������꣬��ȡ������
            stream->bufptr = 0;
            bRead = ReadFile(stream->handle, stream->buf, CRT_BUF_SIZE,
                &stream->bufsize, NULL);
            if (bRead)
                stream->flags &= ~CRT_FILE_ERROR;
            else
                stream->flags |= CRT_FILE_ERROR;
        }

        // �����ɶ�ȡ���ֽ���
        cbio = stream->bufsize - stream->bufptr;
        if (cbio > size)
            cbio = size;

        // �ӻ�����ȡ����
        memcpy(dst, stream->buf + stream->bufptr, cbio);
        // �������ָ��
        dst += cbio;
        ret += cbio;
        stream->bufptr += cbio;
        stream->fileptr += cbio;
    }

    return ret;
}

FILE* __cdecl freopen(const char* path, const char* mode, FILE *stream)
{
    FILE tmp;
    FILE* fp = NULL;
    
    PDLASSERT(NULL != stream);
    fp = fopen(path, mode);
    memcpy(&tmp, stream, sizeof(FILE));
    memcpy(stream, fp, sizeof(FILE));
    memcpy(fp, &tmp, sizeof(FILE));

    if (stdin != stream && stdout != stream && stderr != stream)
        fclose(fp);
    return stream;
}

int __cdecl fseek(FILE* stream, long offset, int origin)
{
    long pos = stream->fileptr;
    long delta;

    PDLASSERT(NULL != stream);

    fflush(stream);
    switch (origin)
    {
    case SEEK_SET:
        pos = offset;
        break;
    case SEEK_CUR:
        pos += offset;
        break;
    case SEEK_END:
        pos = stream->filesize + offset;
        break;
    default:
        return EOF;
    }
    if (pos < 0 || pos > (long)stream->filesize)
        return EOF;

    // ��ҳ�ж�
    delta = stream->fileptr - stream->bufptr - pos;
    if (delta < 0)
        delta = -delta;
    if (delta < CRT_BUF_SIZE)
    {
        // ���ػ�ҳ
        stream->bufptr += pos - stream->fileptr;
    }
    else
    {
        SetFilePointer(stream->handle, offset, NULL, origin);
        stream->bufptr = 0;
        stream->bufsize = 0;
    }

    stream->fileptr = pos;
    return 0;
}

long __cdecl ftell(FILE* stream)
{
    PDLASSERT(NULL != stream);
    return stream->filesize;
}

size_t __cdecl fwrite(
    const void* buffer,
    size_t size,
    size_t count,
    FILE* stream)
{
    size_t ret = 0;
    size_t cbio = 0;
    PCBYTE src = buffer;
    BOOL bWritten;

    PDLASSERT(NULL != buffer && NULL != stream);

    size *= count;
    if (NULL == stream->buf)
    {
        // �޻���д��
        bWritten = WriteFile(stream->handle, buffer, size, &ret, NULL);
        if (bWritten)
            stream->flags &= ~CRT_FILE_ERROR;
        else
            stream->flags |= CRT_FILE_ERROR;
        return ret;
    }

    // ����д���־�������¼
    if (0 == (CRT_FILE_WRITE & stream->flags))
    {
        stream->flags |= CRT_FILE_WRITE;
        stream->bufptr = 0;
        stream->bufsize = CRT_BUF_SIZE;
    }

    while (size > 0)
    {
        if (stream->fileptr == stream->filesize)
            return ret;

        if (stream->bufsize == stream->bufptr)
        {
            // �������������꣬д��������
            stream->bufptr = 0;
            bWritten = WriteFile(stream->handle, stream->buf, stream->bufsize,
                &cbio, NULL);
            if (bWritten)
                stream->flags &= ~CRT_FILE_ERROR;
            else
                stream->flags |= CRT_FILE_ERROR;
        }

        // ������д����ֽ���
        cbio = stream->bufsize - stream->bufptr;
        if (cbio > size)
            cbio = size;

        // �򻺳���д����
        memcpy(stream->buf + stream->bufptr, src, cbio);

        // �������ָ��
        src += cbio;
        ret += cbio;
        stream->bufptr += cbio;
        stream->fileptr += cbio;
        if (stream->fileptr > stream->filesize)
            stream->filesize = stream->fileptr;
    }

    return ret;
}

int __cdecl getc(FILE* stream)
{
    PDLASSERT(NULL != stream);

    if (stream->fileptr == stream->filesize)
        return EOF;

    // ˢ��д�뻺��
    fflush(stream);

    if (stream->bufptr == stream->bufsize)
    {
        // ��ȡ��һ������
        ReadFile(stream->handle, stream->buf, CRT_BUF_SIZE,
            &stream->bufsize, NULL);
        stream->bufptr = 0;
    }

    ++stream->fileptr;
    return stream->buf[stream->bufptr++];
}

int __cdecl ungetc(int c, FILE* stream)
{
    PDLASSERT(NULL != stream);

    if (EOF == c || 0 == stream->fileptr)
        return EOF;

    // ˢ��д�뻺��
    fflush(stream);

    if (0 == stream->bufptr)
    {
        // ���»ع��ļ�����
        SetFilePointer(stream->handle, -CRT_BUF_SIZE, NULL, FILE_CURRENT);
        ReadFile(stream->handle, stream->buf, CRT_BUF_SIZE,
            &stream->bufsize, NULL);
        stream->bufptr = stream->bufsize;
    }

    c &= 0xff;
    if (c == stream->buf[stream->bufptr - 1])
    {
        --stream->bufptr;
        --stream->fileptr;
        return c;
    }
    return EOF;
}

// TODO: fprintf
