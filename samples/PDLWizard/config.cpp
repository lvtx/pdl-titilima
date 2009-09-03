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

void CProjectConfig::OutputCfgDebug(LTxtFile* file)
{
    char str[1024];

    lstrcpyA(str, "\t\t<Configuration");
    file->WriteLn(str);

    if (Unicode == m_CharacterSet)
        lstrcpyA(str, "\t\t\tName=\"DebugU|Win32\"");
    else
        lstrcpyA(str, "\t\t\tName=\"DebugA|Win32\"");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\tOutputDirectory=\"$(SolutionDir)$(ConfigurationName)\"\r\n"
        "\t\t\tIntermediateDirectory=\"$(ConfigurationName)\"");
    file->WriteLn(str);

    wsprintfA(str,
        "\t\t\tConfigurationType=\"%d\"\r\n"
        "\t\t\tCharacterSet=\"%d\"\r\n"
        "\t\t\t>",
        theConfig.Type, m_CharacterSet);
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCPreBuildEventTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCCustomBuildTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCXMLDataGeneratorTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCWebServiceProxyGeneratorTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCMIDLTool\"\r\n"
        "\t\t\t/>");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCCLCompilerTool\"\r\n"
        "\t\t\t\tOptimization=\"0\"\r\n"
        "\t\t\t\tAdditionalIncludeDirectories=\"\"\r\n"
        "\t\t\t\tPreprocessorDefinitions=\"WIN32;_DEBUG;_WINDOWS\"\r\n"
        "\t\t\t\tIgnoreStandardIncludePath=\"false\"\r\n"
        "\t\t\t\tMinimalRebuild=\"true\"\r\n"
        "\t\t\t\tExceptionHandling=\"0\"\r\n"
        "\t\t\t\tBasicRuntimeChecks=\"0\"\r\n"
        "\t\t\t\tRuntimeLibrary=\"1\"\r\n"
        "\t\t\t\tBufferSecurityCheck=\"false\"\r\n"
        "\t\t\t\tTreatWChar_tAsBuiltInType=\"true\"\r\n"
        "\t\t\t\tRuntimeTypeInfo=\"false\"\r\n"
        "\t\t\t\tUsePrecompiledHeader=\"0\"\r\n"
        "\t\t\t\tWarningLevel=\"3\"\r\n"
        "\t\t\t\tDebugInformationFormat=\"4\"\r\n"
        "\t\t\t/>");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCManagedResourceCompilerTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCResourceCompilerTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCPreLinkEventTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCLinkerTool\"");
    file->WriteLn(str);

    lstrcpyA(str, "\t\t\t\tAdditionalDependencies=\"pdl.lib\"");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t\tLinkIncremental=\"2\"\r\n"
        "\t\t\t\tGenerateManifest=\"false\"\r\n"
        "\t\t\t\tManifestFile=\"\"\r\n"
        "\t\t\t\tGenerateDebugInformation=\"true\"");
    file->WriteLn(str);

    wsprintfA(str, "\t\t\t\tSubSystem=\"%d\"\r\n", theConfig.SubSystem);
    lstrcatA(str, "\t\t\t\tOptimizeReferences=\"0\"\r\n"
        "\t\t\t\tEnableCOMDATFolding=\"0\"\r\n"
        "\t\t\t\tTargetMachine=\"1\""
        "\t\t\t/>\r\n");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCALinkTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCManifestTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCXDCMakeTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCBscMakeTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCFxCopTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCAppVerifierTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCPostBuildEventTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t</Configuration>");
    file->WriteLn(str);
}

void CProjectConfig::OutputCfgRelease(LTxtFile* file)
{
    char str[1024];

    lstrcpyA(str, "\t\t<Configuration");
    file->WriteLn(str);

    if (Unicode == m_CharacterSet)
        lstrcpyA(str, "\t\t\tName=\"ReleaseU|Win32\"");
    else
        lstrcpyA(str, "\t\t\tName=\"ReleaseA|Win32\"");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\tOutputDirectory=\"$(SolutionDir)$(ConfigurationName)\"\r\n"
        "\t\t\tIntermediateDirectory=\"$(ConfigurationName)\"");
    file->WriteLn(str);

    wsprintfA(str,
        "\t\t\tConfigurationType=\"%d\"\r\n"
        "\t\t\tCharacterSet=\"%d\"\r\n"
        "\t\t\t>",
        theConfig.Type, m_CharacterSet);
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCPreBuildEventTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCCustomBuildTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCXMLDataGeneratorTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCWebServiceProxyGeneratorTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCMIDLTool\"\r\n"
        "\t\t\t/>");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCCLCompilerTool\"\r\n"
        "\t\t\t\tOptimization=\"2\"\r\n"
        "\t\t\t\tAdditionalIncludeDirectories=\"\"\r\n"
        "\t\t\t\tPreprocessorDefinitions=\"WIN32;NDEBUG;_WINDOWS\"\r\n"
        "\t\t\t\tIgnoreStandardIncludePath=\"false\"\r\n"
        "\t\t\t\tMinimalRebuild=\"false\"\r\n"
        "\t\t\t\tExceptionHandling=\"0\"\r\n"
        "\t\t\t\tBasicRuntimeChecks=\"0\"\r\n"
        "\t\t\t\tRuntimeLibrary=\"0\"\r\n"
        "\t\t\t\tBufferSecurityCheck=\"false\"\r\n"
        "\t\t\t\tTreatWChar_tAsBuiltInType=\"true\"\r\n"
        "\t\t\t\tRuntimeTypeInfo=\"false\"\r\n"
        "\t\t\t\tUsePrecompiledHeader=\"0\"\r\n"
        "\t\t\t\tWarningLevel=\"3\"\r\n"
        "\t\t\t\tDebugInformationFormat=\"0\"\r\n"
        "\t\t\t/>");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCManagedResourceCompilerTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCResourceCompilerTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCPreLinkEventTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCLinkerTool\"");
    file->WriteLn(str);

    lstrcpyA(str, "\t\t\t\tAdditionalDependencies=\"pdl.lib\"");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t\tLinkIncremental=\"1\"\r\n"
        "\t\t\t\tGenerateManifest=\"false\"\r\n"
        "\t\t\t\tManifestFile=\"\"\r\n"
        "\t\t\t\tGenerateDebugInformation=\"false\"");
    file->WriteLn(str);

    wsprintfA(str, "\t\t\t\tSubSystem=\"%d\"\r\n", theConfig.SubSystem);
    lstrcatA(str, "\t\t\t\tOptimizeReferences=\"2\"\r\n"
        "\t\t\t\tEnableCOMDATFolding=\"2\"\r\n"
        "\t\t\t\tTargetMachine=\"1\"\r\n"
        "\t\t\t/>");
    file->WriteLn(str);

    lstrcpyA(str,
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCALinkTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCManifestTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCXDCMakeTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCBscMakeTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCFxCopTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCAppVerifierTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t\t<Tool\r\n"
        "\t\t\t\tName=\"VCPostBuildEventTool\"\r\n"
        "\t\t\t/>\r\n"
        "\t\t</Configuration>");
    file->WriteLn(str);
}

void CProjectConfig::OutputFiles(LTxtFile* file)
{
    char str[1024];

    lstrcpyA(str, "\t</Configurations>\r\n"
        "\t<References>\r\n"
        "\t</References>\r\n"
        "\t<Files>\r\n"
        "\t\t<Filter\r\n"
        "\t\t\tName=\"Source Files\"\r\n"
        "\t\t\tFilter=\"cpp;c;cc;cxx;def;odl;idl;hpj;bat;asm;asmx\"");
    file->WriteLn(str);

    OutputUID(file, "\t\t\tUniqueIdentifier");

    wsprintfA(str,
        "\t\t\t>\r\n"
        "\t\t\t<File\r\n"
        "\t\t\t\tRelativePath=\".\\%s.cpp\"\r\n"
        "\t\t\t\t>\r\n"
        "\t\t\t</File>\r\n"
        "\t\t</Filter>", theConfig.szName);
    file->WriteLn(str);

    lstrcpyA(str, "\t\t<Filter\r\n"
        "\t\t\tName=\"Header Files\"\r\n"
        "\t\t\tFilter=\"h;hpp;hxx;hm;inl;inc;xsd\"");
    file->WriteLn(str);

    OutputUID(file, "\t\t\tUniqueIdentifier");

    lstrcpyA(str, "\t\t\t>\r\n"
        "\t\t</Filter>\r\n"
        "\t\t<Filter\r\n"
        "\t\t\tName=\"Resource Files\"\r\n"
        "\t\t\tFilter=\"rc;ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe;resx;tiff;tif;png;wav\"");
    file->WriteLn(str);

    OutputUID(file, "\t\t\tUniqueIdentifier");

    lstrcpyA(str, "\t\t\t>\r\n"
        "\t\t</Filter>\r\n"
        "\t</Files>\r\n"
        "\t<Globals>\r\n"
        "\t</Globals>\r\n"
        "</VisualStudioProject>");
    file->WriteLn(str);
}

void CProjectConfig::OutputHeader(LTxtFile* file)
{
    char str[1024];

    lstrcpyA(str,
        "<?xml version=\"1.0\" encoding=\"gb2312\"?>\r\n"
        "<VisualStudioProject\r\n"
        "\tProjectType=\"Visual C++\"\r\n"
        "\tVersion=\"9.00\"\r\n"
        "\tName=\"");
    file->Write(str);
    file->Write(theConfig.szName);

    lstrcpy(str, "\"");
    file->WriteLn(str);

    OutputUID(file, "\tProjectGUID");

    lstrcpyA(str, "\tRootNamespace=\"");
    lstrcatA(str, theConfig.szName);
    lstrcatA(str, "\"\r\n\tKeyword=\"Win32Proj\"\r\n"
        "\tTargetFrameworkVersion=\"196613\"\r\n"
        "\t>\r\n"
        "\t<Platforms>\r\n"
        "\t\t<Platform\r\n"
        "\t\t\tName=\"Win32\"\r\n"
        "\t\t/>\r\n"
        "\t</Platforms>\r\n"
        "\t<ToolFiles>\r\n"
        "\t</ToolFiles>\r\n"
        "\t<Configurations>");
    file->WriteLn(str);
}

void CProjectConfig::OutputUID(LTxtFile* file, PCSTR key)
{
    char str[1024];

    GUID guid;
    CoCreateGuid(&guid);
    wsprintfA(str, 
        "%s=\"{%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X}\"",
        key,
        guid.Data1, guid.Data2, guid.Data3, guid.Data4[0], guid.Data4[1],
        guid.Data4[2], guid.Data4[3], guid.Data4[4], guid.Data4[5],
        guid.Data4[6], guid.Data4[7]);
    file->WriteLn(str);
}

void CProjectConfig::SetCharacterSet(CHARACTERSET CharacterSet)
{
    m_CharacterSet = CharacterSet;
}
