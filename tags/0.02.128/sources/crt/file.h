///////////////////////////////////////////////////////////////////////////////
// FileName:    file.h
// Created:     2009/04/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 文件结构定义
///////////////////////////////////////////////////////////////////////////////

#pragma once

// 文件缓冲区大小
#define CRT_BUF_SIZE    4096
// 文本文件
#define CRT_FILE_TEXT   0x00000001
// 用于写入的标志
#define CRT_FILE_WRITE  0x00000010
// 流之中是否有错误
#define CRT_FILE_ERROR  0x80000000

struct _iobuf {
    DWORD flags;    // 文件标志
    HANDLE handle;  // 文件句柄
    DWORD fileptr;  // 文件指针
    DWORD filesize; // 文件大小
    PBYTE buf;      // 缓冲
    DWORD bufptr;   // 缓冲区指针
    DWORD bufsize;  // 缓冲区大小
};
