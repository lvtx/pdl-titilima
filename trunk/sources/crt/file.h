///////////////////////////////////////////////////////////////////////////////
// FileName:    file.h
// Created:     2009/04/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: �ļ��ṹ����
///////////////////////////////////////////////////////////////////////////////

#pragma once

// �ļ���������С
#define CRT_BUF_SIZE    4096
// �ı��ļ�
#define CRT_FILE_TEXT   0x00000001
// ����д��ı�־
#define CRT_FILE_WRITE  0x00000010
// ��֮���Ƿ��д���
#define CRT_FILE_ERROR  0x80000000

struct _iobuf {
    DWORD flags;    // �ļ���־
    HANDLE handle;  // �ļ����
    DWORD fileptr;  // �ļ�ָ��
    DWORD filesize; // �ļ���С
    PBYTE buf;      // ����
    DWORD bufptr;   // ������ָ��
    DWORD bufsize;  // ��������С
};
