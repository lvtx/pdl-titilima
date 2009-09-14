///////////////////////////////////////////////////////////////////////////////
// �ļ�����  config.h
// ����ʱ�䣺2009-01-16
// ���ߣ�    ����
// ��Ȩ���У�Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// ˵����    ����������
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
    void OutputCfgDebug(LXmlParser* cfg, LXmlNode node);
    void OutputCfgRelease(LXmlParser* cfg, LXmlNode node);
    void OutputFiles(LXmlParser* cfg, LXmlNode node);
    LXmlNode OutputHeader(LXmlParser* cfg);
    void SetCharacterSet(CHARACTERSET CharacterSet);
private:
    BOOL CreateFileFromResource(PCSTR lpFile, UINT id);
    void OutputUID(LXmlParser* cfg, LXmlNode node, PCSTR key);
private:
    CHARACTERSET m_CharacterSet;
};
