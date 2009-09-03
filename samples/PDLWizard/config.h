///////////////////////////////////////////////////////////////////////////////
// 文件名：  config.h
// 创建时间：2009-01-16
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    工程配置类
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include <pdl_file.h>
#include <pdl_parser.h>

enum CONFIGURATIONTYPE {
    Application = 1,
    DynamicLinkLibrary = 2,
};

enum CHARACTERSET {
    Unicode = 0x00000001,
    MultiByte = 0x00000002,
};

enum SUBSYSTEM {
    Console = 1,
    Windows = 2,
};

#define CONFIG_ANSI     0x00000001
#define CONFIG_UNICODE  0x00000002
#define CONFIG_MANIFEST 0x00000004

typedef struct _tagConfig {
    CONFIGURATIONTYPE Type;
    SUBSYSTEM SubSystem;
    DWORD Flags;
    CHAR szName[MAX_PATH];
    CHAR szPath[MAX_PATH];
} CONFIG;

extern CONFIG theConfig;
extern LIniParser theIni;

class CProjectConfig
{
public:
    CProjectConfig(void);
public:
    void CreateFiles(void);
    void OutputCfgDebug(LTxtFile* file);
    void OutputCfgRelease(LTxtFile* file);
    void OutputFiles(LTxtFile* file);
    void OutputHeader(LTxtFile* file);
    void SetCharacterSet(CHARACTERSET CharacterSet);
private:
    BOOL CreateFileFromResource(PCSTR lpFile, UINT id);
    void OutputUID(LTxtFile* file, PCSTR key);
private:
    CHARACTERSET m_CharacterSet;
};
