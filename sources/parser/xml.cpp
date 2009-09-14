///////////////////////////////////////////////////////////////////////////////
// FileName:    xml.cpp
// Created:     2009/09/13
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: XML Parser
///////////////////////////////////////////////////////////////////////////////

#include "..\include\pdl_parser.h"
#include "..\include\pdl_file.h"
#include "..\include\pdl_module.h"

///////////////////////////////////////////////////////////////////////////////
// xmlAttrib

class xmlAttrib
{
    friend class xmlNode;
public:
    xmlAttrib(__in PCSTR n, __in PCSTR v)
    {
        m_n = LStringA::AllocString(n);
        m_v = LStringA::AllocString(v);
        m_next = NULL;
    }
    ~xmlAttrib(void)
    {
        LStringA::FreeString(m_n);
        LStringA::FreeString(m_v);
    }
private:
    PSTR m_n;
    PSTR m_v;
    xmlAttrib* m_next;
};

///////////////////////////////////////////////////////////////////////////////
// xmlNode

class xmlNode
{
    friend class LXmlParser;
public:
    xmlNode(__in PCSTR name)
    {
        m_name = LStringA::AllocString(name);
        m_props = NULL;
        m_lastprop = NULL;
    }
    ~xmlNode(void)
    {
        LStringA::FreeString(m_name);

        xmlAttrib* prop = m_props;
        while (NULL != prop)
        {
            xmlAttrib* del = prop;
            prop = prop->m_next;
            delete del;
        }
    }
    void AddProperty(__in PCSTR name, __in PCSTR value)
    {
        xmlAttrib* attr = new xmlAttrib(name, value);
        if (NULL == m_props)
        {
            m_props = attr;
            m_lastprop = attr;
        }
        else
        {
            m_lastprop->m_next = attr;
        }
    }
    BOOL GetProperty(__in PCSTR name, __out LStringA* value)
    {
        xmlAttrib* attr = FindProperty(name);
        if (NULL == attr)
            return FALSE;

        value->Copy(attr->m_v);
        return TRUE;
    }
    BOOL GetProperty(__in PCSTR name, __out LStringW* value)
    {
        xmlAttrib* attr = FindProperty(name);
        if (NULL == attr)
            return FALSE;

        value->Copy(attr->m_v);
        return TRUE;
    }
    void Output(__in LTxtFile* file)
    {
        if (LXmlParser::Prolog == m_type)
            file->PutChar('?');

        file->Write(m_name);
        xmlAttrib* attr = m_props;
        while (NULL != attr)
        {
            file->PrintF(" %s=\"%s\"", attr->m_n, attr->m_v);
            attr = attr->m_next;
        }

        if (LXmlParser::Prolog == m_type)
            file->PutChar('?');
    }
    BOOL SetProperty(__in PCSTR name, __in PCSTR value)
    {
        if ('\0' == *name)
            return FALSE;

        char nullstr = '\0';
        if (NULL == value)
            value = &nullstr;

        xmlAttrib* attr = FindProperty(name);
        if (NULL == attr)
        {
            AddProperty(name, value);
            return TRUE;
        }

        LStringA::FreeString(attr->m_v);
        attr->m_v = LStringA::AllocString(value);
        return TRUE;
    }
    BOOL SetProperty(__in PCSTR name, __in PCWSTR value)
    {
        LStringA strA = value;
        return SetProperty(name, strA);
    }
private:
    xmlAttrib* FindProperty(__in PCSTR name)
    {
        xmlAttrib* attr = m_props;
        while (NULL != attr)
        {
            if (0 == lstrcmpiA(name, attr->m_n))
                break;

            attr = attr->m_next;
        }
        return attr;
    }
private:
    PSTR m_name;
    LXmlParser::NodeType m_type;
    xmlAttrib* m_props;
    xmlAttrib* m_lastprop;
};

///////////////////////////////////////////////////////////////////////////////
// LXmlFile

class LXmlFile : public LXmlStream
{
public:
    LXmlFile(__in LTxtFile* file)
    {
        m_file = file;
    }
    BOOL Eos(void)
    {
        return m_file->Eof();
    }
    int GetChar(void)
    {
        return m_file->GetChar();
    }
private:
    LTxtFile* m_file;
};

///////////////////////////////////////////////////////////////////////////////
// LXmlParser

void xmlFree(void* ptr)
{
    xmlNode** node = (xmlNode**)ptr;
    delete *node;
}

LXmlParser::LXmlParser(__in ILock* lock /* = NULL */) : LPtrTree()
{
    LPtrTree::Create(sizeof(xmlNode*), NULL, xmlFree, lock);
}

LXmlParser::~LXmlParser(void)
{
    Close();
}

void LXmlParser::Close(void)
{
    LPtrTree::Clear();
}

LXmlNode LXmlParser::CreateNode(
    __in LXmlNode parent,
    __in NodeType type,
    __in PCSTR name,
    __in DWORD dwOrder)
{
    xmlNode* node = new xmlNode(name);
    if (NULL == node)
        return NULL;

    node->m_type = type;
    return LPtrTree::AddChild(parent, &node, dwOrder);
}

LXmlNode LXmlParser::CreateNode(
    __in LXmlNode parent,
    __in NodeType type,
    __in PCWSTR name,
    __in DWORD dwOrder)
{
    LStringA strA = name;
    return CreateNode(parent, type, strA, dwOrder);
}

int LXmlParser::GetChar(__in LXmlStream* s)
{
    int ret = ' ';
    while (' ' == ret || '\t' == ret || '\r' == ret || '\n' == ret)
    {
        if (s->Eos())
            return LEOF;

        ret = s->GetChar();
    }
    return ret;
}

LXmlNode LXmlParser::GetChild(__in LXmlNode node, __in DWORD type)
{
    return LPtrTree::GetChild(node, type);
}

LXmlNode LXmlParser::GetNextSibling(__in LXmlNode node)
{
    return LPtrTree::GetNextSibling(node);
}

BOOL LXmlParser::GetNodeName(__in LXmlNode node, __out LStringA* name)
{
    if (NULL == node)
        return FALSE;

    xmlNode* p = NULL;
    if (!LPtrTree::GetAt(node, &p))
        return FALSE;

    name->Copy(p->m_name);
    return TRUE;
}

BOOL LXmlParser::GetNodeName(__in LXmlNode node, __out LStringW* name)
{
    if (NULL == node)
        return FALSE;

    xmlNode* p = NULL;
    if (!LPtrTree::GetAt(node, &p))
        return FALSE;

    name->Copy(p->m_name);
    return TRUE;
}

BOOL LXmlParser::GetNodeProperty(
    __in LXmlNode node,
    __in PCSTR name,
    __out LStringA* value)
{
    if (NULL == node)
        return FALSE;

    xmlNode* p = NULL;
    if (!LPtrTree::GetAt(node, &p))
        return FALSE;

    return p->GetProperty(name, value);
}

BOOL LXmlParser::GetNodeProperty(
    __in LXmlNode node,
    __in PCSTR name,
    __out LStringW* value)
{
    if (NULL == node)
        return FALSE;

    xmlNode* p = NULL;
    if (!LPtrTree::GetAt(node, &p))
        return FALSE;

    return p->GetProperty(name, value);
}

BOOL LXmlParser::GetNodeType(__in LXmlNode node, __out NodeType* type)
{
    if (NULL == node)
        return FALSE;

    xmlNode* p = NULL;
    if (!LPtrTree::GetAt(node, &p))
        return FALSE;

    *type = p->m_type;
    return TRUE;
}

LXmlNode LXmlParser::GetPrevSibling(__in LXmlNode node)
{
    return LPtrTree::GetPrevSibling(node);
}

int LXmlParser::GetString(
    __in LXmlStream* s,
    __out LStringA* str,
    __in PCSTR lpEnd,
    __out char* chEnd)
{
    int ret = 0;
    char ch = s->GetChar();
    while (!IsEndChar(lpEnd, ch))
    {
        if (s->Eos())
            return 0;

        str->Append(ch);
        ch = s->GetChar();
        ++ret;
    }

    *chEnd = ch;
    return ret;
}

BOOL LXmlParser::IsEndChar(__in PCSTR lpEnd, __in char ch)
{
    while ('\0' != *lpEnd)
    {
        if (*lpEnd == ch)
            return TRUE;
        ++lpEnd;
    }
    return FALSE;
}

BOOL LXmlParser::Open(__in PCSTR lpszFileName)
{
    Clear();
    if (NULL == lpszFileName)
        return FALSE;

    LStringA path;
    if (LFile::IsFullPathName(lpszFileName))
    {
        path = lpszFileName;
    }
    else
    {
        LAppModule::GetAppPath(&path);
        path += lpszFileName;
    }
    if (!LFile::Exists(path))
        return FALSE;

    LTxtFile file;
    if (!file.Open(path, LTxtFile::modeReadWrite))
        return FALSE;

    LXmlFile stream(&file);
    if (Parse(&stream))
        return TRUE;

    Clear();
    return FALSE;
}

BOOL LXmlParser::Open(__in PCWSTR lpszFileName)
{
    Clear();
    if (NULL == lpszFileName)
        return FALSE;

    LStringW path;
    if (LFile::IsFullPathName(lpszFileName))
    {
        path = lpszFileName;
    }
    else
    {
        LAppModule::GetAppPath(&path);
        path += lpszFileName;
    }
    if (!LFile::Exists(path))
        return FALSE;

    LTxtFile file;
    if (!file.Open(path, LTxtFile::modeReadWrite))
        return FALSE;

    LXmlFile stream(&file);
    if (Parse(&stream))
        return TRUE;

    Clear();
    return FALSE;
}

void LXmlParser::OutputIndent(
    __in LTxtFile* file,
    __in int level,
    __in PCSTR strIndent)
{
    for (int i = 0; i < level; ++i)
        file->Write(strIndent);
}

BOOL LXmlParser::Parse(__in LXmlStream* s)
{
    LStringA str, strValue;
    LIterator it = LT_ROOT;
    xmlNode* node = NULL;
    LStack<LIterator> stack;
    int len = 0;
    BOOL bPush = TRUE;
    BOOL ret = TRUE;

    char ch = GetChar(s);
    while (!s->Eos())
    {
        str.Empty();
        if ('<' == ch)
        {
            bPush = TRUE;
            ch = s->GetChar();
            if ('/' == ch)
            {
                // 结点闭合，如 </node>
                if (0 == GetString(s, &str, ">", &ch))
                {
                    ret = FALSE;
                    break;
                }

                LStringA name;
                if (!GetNodeName(it, &name) || 0 != lstrcmpA(name, str))
                {
                    ret = FALSE;
                    break;
                }

                // 弹出原有的父结点
                stack.Pop(&it);
            }
            else if ('?' == ch)
            {
                // 新序言结点，如 <?xml ... ?>
                if (0 == GetString(s, &str, " \t?", &ch))
                {
                    ret = FALSE;
                    break;
                }
                if ('?' == ch && '>' != s->GetChar())
                {
                    ret = FALSE;
                    break;
                }

                node = new xmlNode(str);
                node->m_type = LXmlParser::Prolog;
                bPush = FALSE;
            }
            else
            {
                // 普通结点，如 <node ... >
                str.Append(ch);
                if (0 == GetString(s, &str, " \t/>", &ch))
                {
                    ret = FALSE;
                    break;
                }
                if ('/' == ch)
                {
                    // 闭合结点，如 <node ... />
                    ch = s->GetChar();
                    if ('>' != ch)
                    {
                        ret = FALSE;
                        break;
                    }
                    bPush = FALSE;
                }

                node = new xmlNode(str);
                node->m_type = LXmlParser::Element;
                if ('>' == ch)
                    continue;
            }
        }
        else if ('>' == ch)
        {
            // 结点末尾，加入数据
            LIterator add = LPtrTree::AddChild(it, &node, LT_LAST);
            if (bPush)
            {
                // 原有父结点入栈
                stack.Push(it);
                // 当前结点成为新的父结点
                it = add;
                node = NULL;
            }
        }
        else if ('?' == ch)
        {
            // 序言结点的闭合
            ch = GetChar(s);
            if ('>' != ch)
            {
                ret = FALSE;
                break;
            }
            if (NULL == node || LXmlParser::Prolog != node->m_type)
            {
                ret = FALSE;
                break;
            }

            bPush = FALSE;
            continue;
        }
        else if ('/' == ch)
        {
            // 普通结点的闭合
            ch = s->GetChar();
            if ('>' != ch)
            {
                ret = FALSE;
                break;
            }
            if (NULL == node || LXmlParser::Element != node->m_type)
            {
                ret = FALSE;
                break;
            }

            bPush = FALSE;
            continue;
        }
        else if (NULL != node)
        {
            // 属性名称
            str.Append(ch);
            len = GetString(s, &str, "=", &ch);
            if (0 == len)
                return FALSE;

            // 属性值
            ch = GetChar(s);
            if ('"' != ch)
                return FALSE;
            strValue.Empty();
            len = GetString(s, &strValue, "\"", &ch);
            if (0 == len)
                return FALSE;

            str.Trim(" \t");
            node->AddProperty(str, strValue);
        }
        else
        {
            // 普通文本结点
            str.Append(ch);
            len = GetString(s, &str, "<", &ch);
            if (0 == len)
                return FALSE;

            str.Trim(" \t\r\n");
            node = new xmlNode(str);
            node->m_type = LXmlParser::Text;
            LPtrTree::AddChild(it, &node, LT_LAST);
            continue;
        }
        ch = GetChar(s);
    }

    if (!ret)
    {
        if (NULL != node)
            delete node;
    }
    return ret;
}

BOOL LXmlParser::Save(__in PCSTR lpszFileName, __in PCSTR strIndent)
{
    if (NULL == lpszFileName)
        return FALSE;

    LStringA path;
    if (LFile::IsFullPathName(lpszFileName))
    {
        path = lpszFileName;
    }
    else
    {
        LAppModule::GetAppPath(&path);
        path += lpszFileName;
    }

    LTxtFile file;
    if (!file.Open(path, LTxtFile::modeReset))
        return FALSE;

    if (NULL == strIndent)
        strIndent = "    ";
    Save(&file, strIndent);
    return TRUE;
}

BOOL LXmlParser::Save(__in PCWSTR lpszFileName, __in PCSTR strIndent)
{
    if (NULL == lpszFileName)
        return FALSE;

    LStringW path;
    if (LFile::IsFullPathName(lpszFileName))
    {
        path = lpszFileName;
    }
    else
    {
        LAppModule::GetAppPath(&path);
        path += lpszFileName;
    }

    LTxtFile file;
    if (!file.Open(path, LTxtFile::modeReset))
        return FALSE;

    if (NULL == strIndent)
        strIndent = "    ";
    Save(&file, strIndent);
    return TRUE;
}

void LXmlParser::Save(__in LTxtFile* file, __in PCSTR strIndent)
{
    if (NULL == m_itRootFirst)
        return;

    xmlNode* p = NULL;
    LXmlNode node = m_itRootFirst;
    int level = 0;
    LStack<LXmlNode> stack;
    while (NULL != node)
    {
        // 输出结点本身
        LPtrTree::GetAt(node, &p);
        OutputIndent(file, level, strIndent);
        file->PutChar('<');
        p->Output(file);

        LXmlNode child = GetChild(node, XML_FIRST);
        if (NULL != child)
        {
            stack.Push(node);
            node = child;
            file->WriteLn(">\r\n");
            ++level;
            continue;
        }

        file->WriteLn("/>\r\n");
        node = GetNextSibling(node);
        if (NULL == node)
        {
            stack.Pop(&node);
            --level;
        }
    }
}

BOOL LXmlParser::SetNodeProperty(
    __in LXmlNode node,
    __in PCSTR name,
    __in PCSTR value)
{
    if (NULL == node || NULL == name)
        return FALSE;

    xmlNode* p = NULL;
    if (!LPtrTree::GetAt(node, &p))
        return FALSE;

    return p->SetProperty(name, value);
}

BOOL LXmlParser::SetNodeProperty(
    __in LXmlNode node,
    __in PCSTR name,
    __in PCWSTR value)
{
    if (NULL == node || NULL == name)
        return FALSE;

    xmlNode* p = NULL;
    if (!LPtrTree::GetAt(node, &p))
        return FALSE;

    return p->SetProperty(name, value);
}

BOOL LXmlParser::SetNodeProperty(
    __in LXmlNode node,
    __in PCSTR name,
    __in int value)
{
    LStringA strA;
    strA.Format("%d", value);
    return SetNodeProperty(node, name, strA);
}
