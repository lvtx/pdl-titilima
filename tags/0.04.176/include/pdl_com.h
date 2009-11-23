/**
 * \file pdl_com.h
 * \brief PDL COM ֧���ļ�
 * \details ����ļ��а����˶� COM �ĸ�������֧�֣�
 *   \li \c LAxCtrl PDL ActiveX �ؼ���
 *   \li \c LBStr PDL BSTR ��װ��
 *   \li \c LComInit PDL COM ��ʼ����
 *   \li \c LComPtr PDL COM ����ָ����
 *   \li \c LComQIPtr PDL COM ����ָ����
 *   \li \c LOleInit PDL OLE ��ʼ����
 *   \li \c LVariant PDL VARIANT ��װ��
 */

#pragma once

#include "pdl_window.h"
#include <comdef.h>

/**
 * QueryInterface ����ת����
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
 * \brief PDL ActiveX �ؼ��ࡣ
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
     * ���� ActiveX ������
     * @param [in] x ���ڵĺ����ꡣ
     * @param [in] y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] id ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in UINT id, __in PVOID lpParam);

    /**
     * ���� ActiveX ������
     * @param [in] rc ���ڵľ��Ρ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] id ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in LPCRECT rc, __in HWND hWndParent, __in UINT id,
        __in PVOID lpParam);

    /**
     * �������д���һ�� ActiveX �ؼ���
     * @param [in] clsid Ŀ��ؼ�������� ID��
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateAxCtrl(__in REFCLSID clsid);

    /**
     * �����¼��ӿڡ�
     * @param [in] iid Ҫ���ӵĽӿ� ID��
     * @param [in] pSink Ҫ���ӵĽӿ�ָ�롣����������Ϊ NULL�������� LAxCtrl ��������
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Connect(__in REFIID iid, __in_opt IUnknown *pSink = NULL);

    /**
     * ���� ActiveX �ؼ���
     */
    void DestroyAxCtrl(void);

    /**
     * �Ͽ��¼����ӡ�
     * @param [in] iid Ҫ�Ͽ����ӵĽӿ� ID��
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Disconnect(__in REFIID iid);

    /**
     * ��ѯ�ؼ��ӿڡ�
     * @param [in] iid Ҫ��ѯ�Ľӿ� ID��
     * @param [out] ��ѯ�Ľ����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
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
     * ActiveX �ؼ��Ķ���ӿ�
     */
    IOleObject* m_pObject;
    /**
     * ��������ʱ�� cookie
     */
    DWORD m_dwCookie;
};

/**
 * \class LBSTR
 * \brief PDL BSTR ��װ�ࡣ
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
     * �� LBStr �����ŵ�һ�� BSTR �ַ����ϡ�
     * @param [in] bstr һ����Ч�� BSTR �ַ�����
     */
    void Attach(__in BSTR bstr);

    /**
     * ����һ���ַ�����
     * @param [in] lpString һ����Ч���ַ�����
     */
    void Copy(__in PCSTR lpString);

    /**
     * ����һ���ַ�����
     * @param [in] lpString һ����Ч�Ŀ��ַ�����
     */
    void Copy(__in PCWSTR lpString);

    /**
     * ��� LBStr ����� BSTR �ַ����İ󶨡�
     * @return LBSTR ԭ�󶨵��ַ�����
     */
    BSTR Detach(void);

    /**
     * ��ȡ BSTR �ַ����ĳ��ȡ�
     * @return BSTR �ַ����ĳ��ȡ�
     */
    UINT GetLength(void);

    /**
     * �ж� BSTR �ַ����Ƿ�Ϊ�ա�
     * @return ��� BSTR �ַ���Ϊ���򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL IsEmpty(void);
protected:
    /**
     * BSTR �ַ���ָ��
     */
    BSTR m_str;
};

/**
 * \class LComInit
 * \brief �����Զ����� COM �ĳ�ʼ���ͷ���ʼ����
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
 * \brief �������� LComPtr ���ü������ࡣ
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
 * \brief PDL COM ����ָ�롣
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
 * \brief �����˲�ѯ���ܵ� COM ����ָ�롣
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
 * \brief PDL OLE �Զ���ʼ���ࡣ
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
 * \brief PDL VARIANT ��װ�ࡣ
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
     * �����ֵ��
     */
    HRESULT Clear(void);
    /**
     * ��������ֵ��
     */
    void put_Date(__in DATE dt);
    /**
     * ����˫����ֵ��
     */
    void put_Double(__in double r8);
};
