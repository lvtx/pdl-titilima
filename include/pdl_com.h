/**
 * \file pdl_com.h
 * \brief PDL COM 支持文件
 * \details 这个文件中包含了对 COM 的各种特性支持：
 *   \li \c LAxCtrl PDL ActiveX 控件类
 *   \li \c LBStr PDL BSTR 封装类
 *   \li \c LComInit PDL COM 初始化类
 *   \li \c LComPtr PDL COM 智能指针类
 *   \li \c LComQIPtr PDL COM 智能指针类
 *   \li \c LOleInit PDL OLE 初始化类
 *   \li \c LVariant PDL VARIANT 封装类
 */

#pragma once

#include "pdl_window.h"
#include <comdef.h>

/**
 * QueryInterface 类型转换。
 */
template <typename T>
PDLINLINE T* qi_cast(__in IUnknown* pUnk)
{
    T *p = NULL;
    if (SUCCEEDED(pUnk->QueryInterface(__uuidof(T), (PVOID*)&p)))
        return p;
    else
        return NULL;
}

/**
 * \class LAxCtrl
 * \brief PDL ActiveX 控件类。
 */

class LAxCtrl : public LWindow
              , public IDispatch
              , public IStorage
              , public IOleInPlaceFrame
              , public IOleClientSite
              , public IOleInPlaceSite
{
public:
    LAxCtrl(void);
    virtual ~LAxCtrl(void);
public:

    /**
     * 创建 ActiveX 容器。
     * @param [in] x 窗口的横坐标。
     * @param [in] y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] id 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in UINT id, __in PVOID lpParam);

    /**
     * 创建 ActiveX 容器。
     * @param [in] rc 窗口的矩形。
     * @param [in] hWndParent 父窗口。
     * @param [in] id 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in LPCRECT rc, __in HWND hWndParent, __in UINT id,
        __in PVOID lpParam);

    /**
     * 在容器中创建一个 ActiveX 控件。
     * @param [in] clsid 目标控件的组件类 ID。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateAxCtrl(__in REFCLSID clsid);

    /**
     * 连接事件接口。
     * @param [in] iid 要连接的接口 ID。
     * @param [in] pSink 要连接的接口指针。如果这个参数为 NULL，则连接 LAxCtrl 对象自身。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Connect(__in REFIID iid, __in_opt IUnknown *pSink = NULL);

    /**
     * 销毁 ActiveX 控件。
     */
    void DestroyAxCtrl(void);

    /**
     * 断开事件连接。
     * @param [in] iid 要断开连接的接口 ID。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Disconnect(__in REFIID iid);

    /**
     * 查询控件接口。
     * @param [in] iid 要查询的接口 ID。
     * @param [out] 查询的结果。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL QueryCtrl(__in REFIID iid, __out PVOID* ppv);

protected:
    STDMETHOD(QueryInterface)(__in REFIID riid, __out PVOID* ppvObject);
    STDMETHOD_(ULONG, AddRef)(void);
    STDMETHOD_(ULONG, Release)(void);
    STDMETHOD(GetTypeInfoCount)(__out UINT *pctinfo);
    STDMETHOD(GetTypeInfo)(__in UINT iTInfo, __in LCID lcid,
        __out ITypeInfo **ppTInfo);
    STDMETHOD(GetIDsOfNames)(__in REFIID riid, __in LPOLESTR *rgszNames,
        __in UINT cNames, __in LCID lcid, __out DISPID *rgDispId);
    STDMETHOD(Invoke)(__in DISPID dispIdMember, __in REFIID riid,
        __in LCID lcid, __in WORD wFlags, __inout DISPPARAMS *pDispParams,
        __out VARIANT *pVarResult, __out EXCEPINFO *pExcepInfo,
        __out UINT *puArgErr);
    STDMETHOD(CreateStream)(__in const OLECHAR *pwcsName,
        __in DWORD grfMode, __in DWORD reserved1, __in DWORD reserved2,
        __out IStream **ppstm);
    STDMETHOD(OpenStream)(__in const OLECHAR *pwcsName,
        __in_opt PVOID reserved1, __in DWORD grfMode, __in DWORD reserved2,
        __out IStream **ppstm);
    STDMETHOD(CreateStorage)(__in const OLECHAR *pwcsName,
        __in DWORD grfMode, __in DWORD reserved1, __in DWORD reserved2,
        __out IStorage **ppstg);
    STDMETHOD(OpenStorage)(__in_opt const OLECHAR *pwcsName,
        __in_opt IStorage *pstgPriority, __in DWORD grfMode,
        __in_opt SNB snbExclude, __in DWORD reserved, __out IStorage **ppstg);
    STDMETHOD(CopyTo)(__in DWORD ciidExclude,
        __in_opt const IID *rgiidExclude, __in_opt SNB snbExclude,
        __in_opt IStorage *pstgDest);
    STDMETHOD(MoveElementTo)(__in const OLECHAR *pwcsName,
        __in_opt IStorage *pstgDest, __in const OLECHAR *pwcsNewName,
        __in DWORD grfFlags);
    STDMETHOD(Commit)(__in DWORD grfCommitFlags);
    STDMETHOD(Revert)(void);
    STDMETHOD(EnumElements)(__in DWORD reserved1,
        __in_opt PVOID reserved2, __in DWORD reserved3,
        __out IEnumSTATSTG **ppenum);
    STDMETHOD(DestroyElement)(__in const OLECHAR *pwcsName);
    STDMETHOD(RenameElement)(__in const OLECHAR *pwcsOldName,
        __in const OLECHAR *pwcsNewName);
    STDMETHOD(SetElementTimes)(__in_opt const OLECHAR *pwcsName,
        __in_opt const FILETIME *pctime, __in_opt const FILETIME *patime,
        __in_opt const FILETIME *pmtime);
    STDMETHOD(SetClass)(__in REFCLSID clsid);
    STDMETHOD(SetStateBits)(__in DWORD grfStateBits, __in DWORD grfMask);
    STDMETHOD(Stat)(__out STATSTG *pstatstg, __in DWORD grfStatFlag);
    STDMETHOD(GetWindow)(__out HWND* phwnd);
    STDMETHOD(ContextSensitiveHelp)(__in BOOL fEnterMode);
    STDMETHOD(GetBorder)(__out LPRECT lprectBorder);
    STDMETHOD(RequestBorderSpace)(__in_opt LPCBORDERWIDTHS pborderwidths);
    STDMETHOD(SetBorderSpace)(__in_opt LPCBORDERWIDTHS pborderwidths);
    STDMETHOD(SetActiveObject)(__in_opt IOleInPlaceActiveObject* pActiveObject,
        __in_opt LPCOLESTR pszObjName);
    STDMETHOD(InsertMenus)(__in HMENU hmenuShared,
        __inout LPOLEMENUGROUPWIDTHS lpMenuWidths);
    STDMETHOD(SetMenu)(__in HMENU hmenuShared, __in HOLEMENU holemenu,
        __in HWND hwndActiveObject);
    STDMETHOD(RemoveMenus)(__in HMENU hmenuShared);
    STDMETHOD(SetStatusText)(__in_opt LPCOLESTR pszStatusText);
    STDMETHOD(EnableModeless)(__in BOOL fEnable);
    STDMETHOD(TranslateAcceleratorA)(__in LPMSG lpmsg,
        __in WORD wID);
    STDMETHOD(TranslateAcceleratorW)(__in LPMSG lpmsg,
        __in WORD wID);
    STDMETHOD(SaveObject)(void);
    STDMETHOD(GetMoniker)(__in DWORD dwAssign, __in DWORD dwWhichMoniker,
        __out IMoniker** ppmk);
    STDMETHOD(GetContainer)(__out IOleContainer** ppContainer);
    STDMETHOD(ShowObject)(void);
    STDMETHOD(OnShowWindow)(__in BOOL fShow);
    STDMETHOD(RequestNewObjectLayout)(void);
    STDMETHOD(CanInPlaceActivate)(void);
    STDMETHOD(OnInPlaceActivate)(void);
    STDMETHOD(OnUIActivate)(void);
    STDMETHOD(GetWindowContext)(__out IOleInPlaceFrame** ppFrame,
        __out IOleInPlaceUIWindow** ppDoc, __out LPRECT lprcPosRect,
        __out LPRECT lprcClipRect, __inout LPOLEINPLACEFRAMEINFO lpFrameInfo);
    STDMETHOD(Scroll)(__in SIZE scrollExtant);
    STDMETHOD(OnUIDeactivate)(__in BOOL fUndoable);
    STDMETHOD(OnInPlaceDeactivate)(void);
    STDMETHOD(DiscardUndoState)(void);
    STDMETHOD(DeactivateAndUndo)(void);
    STDMETHOD(OnPosRectChange)(__in LPCRECT lprcPosRect);

    PDL_DECLARE_MSGMAP();
    DECLARE_SIZE_HANDLER(OnSize);
protected:
    /**
     * ActiveX 控件的对象接口
     */
    IOleObject* m_pObject;
    /**
     * 自身连接时的 cookie
     */
    DWORD m_dwCookie;
};

/**
 * \class LBSTR
 * \brief PDL BSTR 封装类。
 */

class LBStr
{
public:
    LBStr(void);
    LBStr(__in PCSTR lpString);
    LBStr(__in PCWSTR lpString);
    virtual ~LBStr(void);
    LBStr& operator=(__in PCSTR lpString);
    LBStr& operator=(__in PCWSTR lpString);
    operator BSTR(void);
    operator PCWSTR(void);
    BSTR* operator&(void);
public:

    /**
     * 将 LBStr 对象附着到一个 BSTR 字符串上。
     * @param [in] bstr 一个有效的 BSTR 字符串。
     */
    void Attach(__in BSTR bstr);

    /**
     * 复制一个字符串。
     * @param [in] lpString 一个有效的字符串。
     */
    void Copy(__in PCSTR lpString);

    /**
     * 复制一个字符串。
     * @param [in] lpString 一个有效的宽字符串。
     */
    void Copy(__in PCWSTR lpString);

    /**
     * 解除 LBStr 对象对 BSTR 字符串的绑定。
     * @return LBSTR 原绑定的字符串。
     */
    BSTR Detach(void);

    /**
     * 获取 BSTR 字符串的长度。
     * @return BSTR 字符串的长度。
     */
    UINT GetLength(void);

    /**
     * 判断 BSTR 字符串是否为空。
     * @return 如果 BSTR 字符串为空则返回 TRUE，否则返回 FALSE。
     */
    BOOL IsEmpty(void);
protected:
    /**
     * BSTR 字符串指针
     */
    BSTR m_str;
};

/**
 * \class LComInit
 * \brief 用于自动处理 COM 的初始化和反初始化。
 */

class LComInit
{
public:
    LComInit(void)
    {
#ifndef _WIN32_WCE
        ::CoInitialize(NULL);
#endif // _WIN32_WCE
    }
    ~LComInit(void)
    {
#ifndef _WIN32_WCE
        ::CoUninitialize();
#endif // _WIN32_WCE
    }
};

///////////////////////////////////////////////////////////////////////////////
// LComPtr

/**
 * \class _RestrictedPtr
 * \brief 用于限制 LComPtr 引用计数的类。
 */

template <class T>
class _RestrictedPtr : public T
{
private:
    STDMETHOD_(ULONG, AddRef)(void) = 0;
    STDMETHOD_(ULONG, Release)(void) = 0;
};

/**
 * \class LComPtr
 * \brief PDL COM 智能指针。
 */

template <class T>
class LComPtr
{
public:
    LComPtr(void) : m_ptr(NULL) { /* Nothing */ }
    LComPtr(T* p) : m_ptr(p)
    {
        if (NULL != m_ptr)
            m_ptr->AddRef();
    }
    LComPtr(const LComPtr<T>& p) : m_ptr(p.m_ptr)
    {
        if (NULL != m_ptr)
            m_ptr->AddRef();
    }
    ~LComPtr(void)
    {
        if (NULL != m_ptr)
            m_ptr->Release();
    }
    void Release(void)
    {
        IUnknown* p = m_ptr;
        if (NULL != p)
        {
            m_ptr = NULL;
            p->Release();
        }
    }
    operator T*(void) const
    {
        return (T*)m_ptr;
    }
    T& operator*(void) const
    {
        PDLASSERT(NULL != m_ptr);
        return *m_ptr;
    }
    T** operator&(void)
    {
        PDLASSERT(NULL == m_ptr);
        return &m_ptr;
    }
    _RestrictedPtr<T>* operator->(void) const
    {
        PDLASSERT(NULL != m_ptr);
        return (_RestrictedPtr<T>*)m_ptr;
    }
    T* operator=(T* p)
    {
        if (m_ptr != p)
        {
            if (NULL != m_ptr)
                m_ptr->Release();
            m_ptr = p;
            m_ptr->AddRef();
        }
        return m_ptr;
    }
    T* operator=(const LComPtr<T>& p)
    {
        if (m_ptr != p.m_ptr)
        {
            if (NULL != m_ptr)
                m_ptr->Release();
            m_ptr = p.m_ptr;
            m_ptr->AddRef();
        }
        return m_ptr;
    }
    bool operator!(void) const
    {
        return (NULL == m_ptr);
    }
    bool operator<(T* p) const
    {
        return m_ptr < p;
    }
    bool operator==(T* p) const
    {
        return m_ptr == p;
    }
    void Attach(T* p)
    {
        if (NULL != m_ptr)
            m_ptr->Release();
        m_ptr = p;
    }
    T* Detach(void)
    {
        T* p = m_ptr;
        m_ptr = NULL;
        return p;
    }
    template <class Q>
    HRESULT QueryInterface(Q** pp) const
    {
        PDLASSERT(NULL != pp && NULL != *pp);
        return m_ptr->QueryInterface(__uuidof(Q), (void**)pp);
    }
    T* m_ptr;
};

/**
 * \class LComQIPtr
 * \brief 增加了查询功能的 COM 智能指针。
 */

template <class T>
class LComQIPtr : public LComPtr<T>
{
public:
    LComQIPtr(void) : LComPtr<T>(NULL) { /* Nothing */ }
    LComQIPtr(__in T* p) : LComPtr<T>(p) { /* Nothing */ }
    LComQIPtr(__in const LComQIPtr<T>& p)
    {
        if (m_ptr != p.m_ptr)
        {
            if (NULL != m_ptr)
                m_ptr->Release();
            m_ptr = p.m_ptr;
            m_ptr->AddRef();
        }
        return m_ptr;
    }
    LComQIPtr(__in IUnknown* pUnk)
    {
        m_ptr = qi_cast<T>(pUnk);
    }
    void Release(void)
    {
        IUnknown* p = m_ptr;
        if (NULL != p)
        {
            m_ptr = NULL;
            p->Release();
        }
    }
    operator T*(void) const
    {
        return m_ptr;
    }
    T& operator*(void) const
    {
        PDLASSERT(m_ptr != NULL);
        return *m_ptr;
    }
    T** operator&(void)
    {
        PDLASSERT(m_ptr == NULL);
        return &m_ptr;
    }
    _RestrictedPtr<T>* operator->(void) const
    {
        PDLASSERT(m_ptr != NULL);
        return (_RestrictedPtr<T>*)m_ptr;
    }
    T* operator=(T* p)
    {
        if (m_ptr != p)
        {
            if (NULL != m_ptr)
                m_ptr->Release();
            m_ptr = p;
            m_ptr->AddRef();
        }
        return m_ptr;
    }
    T* operator=(const LComQIPtr<T>& p)
    {
        if (m_ptr != p.m_ptr)
        {
            if (NULL != m_ptr)
                m_ptr->Release();
            m_ptr = p.m_ptr;
            m_ptr->AddRef();
        }
        return m_ptr;
    }
    T* operator=(IUnknown* pUnk)
    {
        if (m_ptr != pUnk)
        {
            if (NULL != m_ptr)
                m_ptr->Release();
            m_ptr = qi_cast<T>(pUnk);
        }
        return m_ptr;
    }
};

/**
 * \class LOleInit
 * \brief PDL OLE 自动初始化类。
 */

class LOleInit
{
public:
    LOleInit(void)
    {
        ::OleInitialize(NULL);
    }
    ~LOleInit(void)
    {
        ::OleUninitialize();
    }
};

/**
 * \class LVariant
 * \brief PDL VARIANT 封装类。
 */

class LVariant : public VARIANT
{
public:
    LVariant(void);
    ~LVariant(void);
    LVariant(__in short i2);
    LVariant(__in int i4);
    LVariant(__in float r4);
    LVariant(__in double val, VARTYPE type = VT_R8);
    LVariant(__in BSTR bstr);
    LVariant(__in CY cy);
    LVariant(__in IDispatch* dispatch);
    LVariant& operator=(__in short i2);
    LVariant& operator=(__in int i4);
    LVariant& operator=(__in float r4);
    LVariant& operator=(__in CY cy);
    LVariant& operator=(__in BSTR bstr);
    LVariant& operator=(__in IDispatch* dispatch);
public:
    /**
     * 清空数值。
     */
    HRESULT Clear(void);
    /**
     * 设置日期值。
     */
    void put_Date(__in DATE dt);
    /**
     * 设置双精度值。
     */
    void put_Double(__in double r8);
};
