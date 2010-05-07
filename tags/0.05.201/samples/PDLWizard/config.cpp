///////////////////////////////////////////////////////////////////////////////
// 文件名：  config.cpp
// 创建时间：2009-01-16
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    工程配置类实现
///////////////////////////////////////////////////////////////////////////////

#include <pdl_base.h>
#include "config.h"
#include <comdef.h>
#include <pdl_module.h>

#include "resource.h"

CONFIG theConfig;
LIniParser theIni;

CProjectConfig::CProjectConfig(void)
{
    m_CharacterSet = Unicode;
}

BOOL CProjectConfig::CreateFileFromResource(PCSTR lpFile, UINT id)
{
    LAppModule* theApp = LAppModule::GetApp();
    HRSRC hSrc = theApp->FindResource(MAKEINTRESOURCE(id), _T("TEMPLATE"));
    DWORD dwSize = theApp->SizeofResource(hSrc);
    HGLOBAL hMem = theApp->LoadResource(hSrc);
    PVOID data = LockResource(hMem);

    LFile file;
    if (!file.Create(lpFile, GENERIC_WRITE, 0, CREATE_ALWAYS))
        return FALSE;

    file.Write(data, dwSize);
    return TRUE;
}

void CProjectConfig::CreateFiles(void)
{
    CHAR szFile[MAX_PATH];
    lstrcpyA(szFile, theConfig.szName);
    lstrcatA(szFile, ".cpp");
    switch (theConfig.Type)
    {
    case Application:
        {
            if (Windows == theConfig.SubSystem)
                CreateFileFromResource(szFile, IDR_WIN32APP);
            else
                CreateFileFromResource(szFile, IDR_WIN32CLI);
        }
        break;
    case DynamicLinkLibrary:
        CreateFileFromResource(szFile, IDR_WIN32DLL);
        break;
    }
}

void CProjectConfig::OutputCfgDebug(LXmlParser* cfg, LXmlNode node)
{
    LXmlNode cfgnode = cfg->CreateNode(node, LXmlParser::Element,
        "Configuration", XML_LAST);
    if (Unicode == m_CharacterSet)
        cfg->SetNodeProperty(cfgnode, "Name", "DebugU|Win32");
    else
        cfg->SetNodeProperty(cfgnode, "Name", "DebugA|Win32");
    cfg->SetNodeProperty(cfgnode, "OutputDirectory",
        "$(SolutionDir)$(ConfigurationName)");
    cfg->SetNodeProperty(cfgnode, "IntermediateDirectory",
        "$(ConfigurationName)");
    cfg->SetNodeProperty(cfgnode, "ConfigurationType", theConfig.Type);
    cfg->SetNodeProperty(cfgnode, "CharacterSet", m_CharacterSet);

    LXmlNode toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool",
        XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCPreBuildEventTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCCustomBuildTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCXMLDataGeneratorTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCWebServiceProxyGeneratorTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCMIDLTool");

    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCCLCompilerTool");
    cfg->SetNodeProperty(toolnode, "Optimization", 0);
    cfg->SetNodeProperty(toolnode, "AdditionalIncludeDirectories", "");
    cfg->SetNodeProperty(toolnode, "PreprocessorDefinitions",
        "WIN32;_DEBUG;_WINDOWS");
    cfg->SetNodeProperty(toolnode, "IgnoreStandardIncludePath", "false");
    cfg->SetNodeProperty(toolnode, "MinimalRebuild", "true");
    cfg->SetNodeProperty(toolnode, "ExceptionHandling", 0);
    cfg->SetNodeProperty(toolnode, "BasicRuntimeChecks", 0);
    cfg->SetNodeProperty(toolnode, "RuntimeLibrary", 1);
    cfg->SetNodeProperty(toolnode, "BufferSecurityCheck", "false");
    cfg->SetNodeProperty(toolnode, "TreatWChar_tAsBuiltInType", "true");
    cfg->SetNodeProperty(toolnode, "RuntimeTypeInfo", "false");
    cfg->SetNodeProperty(toolnode, "UsePrecompiledHeader", 0);
    cfg->SetNodeProperty(toolnode, "WarningLevel", 3);
    cfg->SetNodeProperty(toolnode, "DebugInformationFormat", 4);

    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCManagedResourceCompilerTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCResourceCompilerTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCPreLinkEventTool");

    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCLinkerTool");
    cfg->SetNodeProperty(toolnode, "AdditionalDependencies", "pdl.lib");
    cfg->SetNodeProperty(toolnode, "LinkIncremental", 2);
    cfg->SetNodeProperty(toolnode, "GenerateManifest", "false");
    cfg->SetNodeProperty(toolnode, "ManifestFile", "");
    cfg->SetNodeProperty(toolnode, "GenerateDebugInformation", "true");
    cfg->SetNodeProperty(toolnode, "SubSystem",theConfig.SubSystem);
    cfg->SetNodeProperty(toolnode, "OptimizeReferences", 0);
    cfg->SetNodeProperty(toolnode, "EnableCOMDATFolding", 0);
    cfg->SetNodeProperty(toolnode, "TargetMachine", 1);

    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCALinkTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCManifestTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCXDCMakeTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCBscMakeTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCFxCopTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCAppVerifierTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCPostBuildEventTool");
}

void CProjectConfig::OutputCfgRelease(LXmlParser* cfg, LXmlNode node)
{
    LXmlNode cfgnode = cfg->CreateNode(node, LXmlParser::Element,
        "Configuration", XML_LAST);
    if (Unicode == m_CharacterSet)
        cfg->SetNodeProperty(cfgnode, "Name", "ReleaseU|Win32");
    else
        cfg->SetNodeProperty(cfgnode, "Name", "ReleaseA|Win32");
    cfg->SetNodeProperty(cfgnode, "OutputDirectory",
        "$(SolutionDir)$(ConfigurationName)");
    cfg->SetNodeProperty(cfgnode, "IntermediateDirectory",
        "$(ConfigurationName)");
    cfg->SetNodeProperty(cfgnode, "ConfigurationType", theConfig.Type);
    cfg->SetNodeProperty(cfgnode, "CharacterSet", m_CharacterSet);

    LXmlNode toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool",
        XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCPreBuildEventTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCCustomBuildTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCXMLDataGeneratorTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCWebServiceProxyGeneratorTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCMIDLTool");

    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCCLCompilerTool");
    cfg->SetNodeProperty(toolnode, "Optimization", 2);
    cfg->SetNodeProperty(toolnode, "AdditionalIncludeDirectories", "");
    cfg->SetNodeProperty(toolnode, "PreprocessorDefinitions",
        "WIN32;NDEBUG;_WINDOWS");
    cfg->SetNodeProperty(toolnode, "IgnoreStandardIncludePath", "false");
    cfg->SetNodeProperty(toolnode, "MinimalRebuild", "false");
    cfg->SetNodeProperty(toolnode, "ExceptionHandling", 0);
    cfg->SetNodeProperty(toolnode, "BasicRuntimeChecks", 0);
    cfg->SetNodeProperty(toolnode, "RuntimeLibrary", 0);
    cfg->SetNodeProperty(toolnode, "BufferSecurityCheck", "false");
    cfg->SetNodeProperty(toolnode, "TreatWChar_tAsBuiltInType", "true");
    cfg->SetNodeProperty(toolnode, "RuntimeTypeInfo", "false");
    cfg->SetNodeProperty(toolnode, "UsePrecompiledHeader", 0);
    cfg->SetNodeProperty(toolnode, "WarningLevel", 3);
    cfg->SetNodeProperty(toolnode, "DebugInformationFormat", 0);

    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCManagedResourceCompilerTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCResourceCompilerTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCPreLinkEventTool");

    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCLinkerTool");
    cfg->SetNodeProperty(toolnode, "AdditionalDependencies", "pdl.lib");
    cfg->SetNodeProperty(toolnode, "LinkIncremental", 1);
    cfg->SetNodeProperty(toolnode, "GenerateManifest", "false");
    cfg->SetNodeProperty(toolnode, "ManifestFile", "");
    cfg->SetNodeProperty(toolnode, "GenerateDebugInformation", "false");
    cfg->SetNodeProperty(toolnode, "SubSystem",theConfig.SubSystem);
    cfg->SetNodeProperty(toolnode, "OptimizeReferences", 2);
    cfg->SetNodeProperty(toolnode, "EnableCOMDATFolding", 2);
    cfg->SetNodeProperty(toolnode, "TargetMachine", 1);

    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCALinkTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCManifestTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCXDCMakeTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCBscMakeTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCFxCopTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCAppVerifierTool");
    toolnode = cfg->CreateNode(cfgnode, LXmlParser::Element, "Tool", XML_LAST);
    cfg->SetNodeProperty(toolnode, "Name", "VCPostBuildEventTool");
}

void CProjectConfig::OutputFiles(LXmlParser* cfg, LXmlNode node)
{
    LStringA s;

    LXmlNode child = cfg->CreateNode(node, LXmlParser::Element, "References",
        XML_LAST);

    LXmlNode files = cfg->CreateNode(node, LXmlParser::Element, "Files",
        XML_LAST);

    LXmlNode filter = cfg->CreateNode(files, LXmlParser::Element, "Filter",
        XML_LAST);
    cfg->SetNodeProperty(filter, "Name", "Source Files");
    cfg->SetNodeProperty(filter, "Filter",
        "cpp;c;cc;cxx;def;odl;idl;hpj;bat;asm;asmx");
    OutputUID(cfg, filter, "UniqueIdentifier");
    LXmlNode file = cfg->CreateNode(filter, LXmlParser::Element, "File",
        XML_LAST);
    s.Format(".\\%s.cpp", theConfig.szName);
    cfg->SetNodeProperty(file, "RelativePath", s);

    filter = cfg->CreateNode(files, LXmlParser::Element, "Filter",
        XML_LAST);
    cfg->SetNodeProperty(filter, "Name", "Header Files");
    cfg->SetNodeProperty(filter, "Filter", "h;hpp;hxx;hm;inl;inc;xsd");
    OutputUID(cfg, filter, "UniqueIdentifier");

    filter = cfg->CreateNode(files, LXmlParser::Element, "Filter",
        XML_LAST);
    cfg->SetNodeProperty(filter, "Name", "Resource Files");
    cfg->SetNodeProperty(filter, "Filter",
        "rc;ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe;resx;tiff;tif;png;wav");
    OutputUID(cfg, filter, "UniqueIdentifier");

    child = cfg->CreateNode(node, LXmlParser::Element, "Globals",
        XML_LAST);
}

LXmlNode CProjectConfig::OutputHeader(LXmlParser* cfg)
{
    // <?xml version="1.0" encoding="gb2312"?>
    LXmlNode node = cfg->CreateNode(XML_ROOT, LXmlParser::Prolog, "xml",
        XML_FIRST);
    cfg->SetNodeProperty(node, "version", "1.0");
    cfg->SetNodeProperty(node, "encoding", "gb2312");

    node = cfg->CreateNode(XML_ROOT, LXmlParser::Element,
        "VisualStudioProject", XML_LAST);
    cfg->SetNodeProperty(node, "ProjectType", "Visual C++");
    cfg->SetNodeProperty(node, "Version", "9.00");
    cfg->SetNodeProperty(node, "Name", theConfig.szName);
    OutputUID(cfg, node, "ProjectGUID");
    cfg->SetNodeProperty(node, "RootNamespace", theConfig.szName);
    cfg->SetNodeProperty(node, "Keyword", "Win32Proj");
    cfg->SetNodeProperty(node, "TargetFrameworkVersion", "196613");

    LXmlNode child = cfg->CreateNode(node, LXmlParser::Element, "Platforms",
        XML_FIRST);
    child = cfg->CreateNode(child, LXmlParser::Element, "Platform", XML_FIRST);
    cfg->SetNodeProperty(child, "Name", "Win32");

    cfg->CreateNode(node, LXmlParser::Element, "ToolFiles", XML_LAST);
    return node;
}

void CProjectConfig::OutputUID(LXmlParser* cfg, LXmlNode node, PCSTR key)
{
    char str[1024];

    GUID guid;
    CoCreateGuid(&guid);
    wsprintfA(str, "{%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X}",
        guid.Data1, guid.Data2, guid.Data3, guid.Data4[0], guid.Data4[1],
        guid.Data4[2], guid.Data4[3], guid.Data4[4], guid.Data4[5],
        guid.Data4[6], guid.Data4[7]);
    cfg->SetNodeProperty(node, key, str);
}

void CProjectConfig::SetCharacterSet(CHARACTERSET CharacterSet)
{
    m_CharacterSet = CharacterSet;
}
