///////////////////////////////////////////////////////////////////////////////
// FileName:    axctrl.cpp
// Created:     2009/06/05
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: ActiveX ¿Ø¼þÈÝÆ÷ÊµÏÖ
///////////////////////////////////////////////////////////////////////////////

#include "..\..\include\pdl_com.h"
#include "..\..\include\pdl_module.h"

#define AXCTRL_CLASS    _T("LAxCtrl")

LAxCtrl::LAxCtrl(void) : LWindow(), m_pObject(NULL), m_dwCookie(0)
{
    WNDCLASS wc = { 0 };
    wc.hbrBackground = PDL_SYSBRUSH(COLOR_WINDOW);
    wc.hCursor = ::LoadCursor(NULL, IDC_ARROW);
    wc.hInstance = LAppModule::GetApp()->GetInstance();
    wc.lpszClassName = AXCTRL_CLASS;
    LWindow::Register(&wc);
}

LAxCtrl::~LAxCtrl(void)
{
}

BOOL LAxCtrl::Create(
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT id,
    __in PVOID lpParam)
{
    return LWindow::Create(AXCTRL_CLASS, _T(""), WS_CHILD | WS_VISIBLE, x, y,
        nWidth, nHeight, hWndParent, (HMENU)id, lpParam);
}

BOOL LAxCtrl::Create(
    __in LPCRECT rc,
    __in HWND hWndParent,
    __in UINT id,
    __in PVOID lpParam)
{
    return LWindow::Create(AXCTRL_CLASS, _T(""), WS_CHILD | WS_VISIBLE, rc,
        hWndParent, id, lpParam);
}

BOOL LAxCtrl::CreateAxCtrl(__in REFCLSID clsid)
{
    HRESULT hr = OleCreate(clsid, IID_IOleObject, OLERENDER_DRAW, NULL, this,
        this, (PVOID*)&m_pObject);
    if (FAILED(hr))
        return FALSE;

    m_pObject->SetHostNames(L"LAxHost", NULL);
    if (FAILED(OleSetContainedObject(m_pObject, TRUE)))
        return FALSE;

    RECT rcClient;
    GetClientRect(&rcClient);
    hr = m_pObject->DoVerb(OLEIVERB_SHOW, NULL, this, -1, m_hWnd, &rcClient);
    return SUCCEEDED(hr);
}

BOOL LAxCtrl::Connect(__in REFIID iid, __in_opt IUnknown *pSink /* = NULL*/)
{
    if (0 != m_dwCookie)
        return FALSE;

    LComQIPtr<IConnectionPointContainer> pCPC = m_pObject;
    if (!pCPC)
        return FALSE;

    LComPtr<IConnectionPoint> pCP = NULL;
    HRESULT hr = pCPC->FindConnectionPoint(iid, &pCP);
    if (FAILED(hr))
        return FALSE;

    if (NULL == pSink)
        pSink = (IDispatch*)this;
    hr = pCP->Advise(pSink, &m_dwCookie);
    return SUCCEEDED(hr);
}

void LAxCtrl::DestroyAxCtrl(void)
{
    if (NULL != m_pObject)
    {
        m_pObject->Close(OLECLOSE_NOSAVE);
        m_pObject->Release();
        m_pObject = NULL;
    }
}

BOOL LAxCtrl::Disconnect(__in REFIID iid)
{
    LComQIPtr<IConnectionPointContainer> pCPC = m_pObject;
    if (!pCPC)
        return FALSE;

    LComPtr<IConnectionPoint> pCP = NULL;
    HRESULT hr = pCPC->FindConnectionPoint(iid, &pCP);
    if (FAILED(hr))
        return FALSE;

    hr = pCP->Unadvise(m_dwCookie);
    m_dwCookie = 0;
    return SUCCEEDED(hr);
}

BOOL LAxCtrl::QueryCtrl(__in REFIID iid, void** ppv)
{
    return SUCCEEDED(m_pObject->QueryInterface(iid, ppv));
}

// IUnknown

STDMETHODIMP_(ULONG) LAxCtrl::AddRef(void)
{
    return 1;
}

STDMETHODIMP_(ULONG) LAxCtrl::Release(void)
{
    return 1;
}

STDMETHODIMP LAxCtrl::QueryInterface(__in REFIID iid, __out PVOID* ppvObject)
{
    if (iid == IID_IUnknown)
    {
        *ppvObject = (IDispatch*)this;
        return S_OK;
    }
    else if (iid == IID_IDispatch)
    {
        *ppvObject = (IDispatch*)this;
        return S_OK;
    }
    else if (iid == IID_IStorage)
    {
        *ppvObject = (IStorage*)this;
        return S_OK;
    }
    else if (iid == IID_IOleInPlaceFrame)
    {
        *ppvObject = (IOleInPlaceFrame*)this;
        return S_OK;
    }
    else if (iid == IID_IOleClientSite)
    {
        *ppvObject = (IOleClientSite*)this;
        return S_OK;
    }
    else if (iid == IID_IOleInPlaceSite)
    {
        *ppvObject = (IOleInPlaceSite*)this;
        return S_OK;
    }

    *ppvObject = NULL;
    return E_NOINTERFACE;
}

// IDispatch

STDMETHODIMP LAxCtrl::GetTypeInfoCount(__out UINT *pctinfo)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::GetTypeInfo(
    __in UINT iTInfo,
    __in LCID lcid,
    __out ITypeInfo **ppTInfo)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::GetIDsOfNames(
    __in REFIID riid,
    __in LPOLESTR* rgszNames,
    __in UINT cNames,
    __in LCID lcid,
    __out DISPID *rgDispId)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::Invoke(
    __in DISPID dispIdMember,
    __in REFIID riid,
    __in LCID lcid,
    __in WORD wFlags,
    __inout DISPPARAMS *pDispParams,
    __out VARIANT *pVarResult,
    __out EXCEPINFO *pExcepInfo,
    __out UINT *puArgErr)
{
    return E_NOTIMPL;
}

// IStorage

STDMETHODIMP LAxCtrl::CreateStream(
    __in const OLECHAR *pwcsName,
    __in DWORD grfMode,
    __in DWORD reserved1,
    __in DWORD reserved2,
    __out IStream** ppstm)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::OpenStream(
    __in const OLECHAR* pwcsName,
    __in_opt PVOID reserved1,
    __in DWORD grfMode,
    __in DWORD reserved2,
    __out IStream** ppstm)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::CreateStorage(
    __in const OLECHAR* pwcsName,
    __in DWORD grfMode,
    __in DWORD reserved1,
    __in DWORD reserved2,
    __out IStorage** ppstg)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::OpenStorage(
    __in const OLECHAR* pwcsName,
    __in_opt IStorage* pstgPriority,
    __in DWORD grfMode,
    __in_opt SNB snbExclude,
    __in DWORD reserved,
    __out IStorage** ppstg)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::CopyTo(
    __in DWORD ciidExclude,
    __in_opt const IID* rgiidExclude,
    __in_opt SNB snbExclude,
    __in_opt IStorage* pstgDest)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::MoveElementTo(
    __in const OLECHAR* pwcsName,
    __in_opt IStorage* pstgDest,
    __in const OLECHAR* pwcsNewName,
    __in DWORD grfFlags)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::Commit(__in DWORD grfCommitFlags)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::Revert(void)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::EnumElements(
    __in DWORD reserved1,
    __in_opt PVOID reserved2,
    __in DWORD reserved3,
    __out IEnumSTATSTG** ppenum)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::DestroyElement(__in const OLECHAR* pwcsName)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::RenameElement(
    __in const OLECHAR* pwcsOldName,
    __in const OLECHAR* pwcsNewName)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::SetElementTimes(
    __in_opt const OLECHAR* pwcsName,
    __in_opt const FILETIME* pctime,
    __in_opt const FILETIME* patime,
    __in_opt const FILETIME* pmtime)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::SetClass(__in REFCLSID clsid)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::SetStateBits(__in DWORD grfStateBits, __in DWORD grfMask)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::Stat(__out STATSTG* pstatstg, __in DWORD grfStatFlag)
{
    return E_NOTIMPL;
}

// IOleInPlaceFrame

STDMETHODIMP LAxCtrl::GetWindow(__out HWND* phwnd)
{
    *phwnd = m_hWnd;
    return S_OK;
}

STDMETHODIMP LAxCtrl::ContextSensitiveHelp(__in BOOL fEnterMode)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::GetBorder(__out LPRECT lprectBorder)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::RequestBorderSpace(
    __in_opt LPCBORDERWIDTHS pborderwidths)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::SetBorderSpace(__in_opt LPCBORDERWIDTHS pborderwidths)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::SetActiveObject(
    __in_opt IOleInPlaceActiveObject *pActiveObject,
    __in_opt LPCOLESTR pszObjName)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::InsertMenus(
    __in HMENU hmenuShared,
    __inout LPOLEMENUGROUPWIDTHS lpMenuWidths)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::SetMenu(
    __in HMENU hmenuShared,
    __in HOLEMENU holemenu,
    __in HWND hwndActiveObject)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::RemoveMenus(__in HMENU hmenuShared)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::SetStatusText(__in_opt LPCOLESTR pszStatusText)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::EnableModeless(__in BOOL fEnable)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::TranslateAcceleratorA(__in LPMSG lpmsg, __in WORD wID)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::TranslateAcceleratorW(__in LPMSG lpmsg, __in WORD wID)
{
    return E_NOTIMPL;
}

// IOleClientSite

STDMETHODIMP LAxCtrl::SaveObject(void)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::GetMoniker(
    __in DWORD dwAssign,
    __in DWORD dwWhichMoniker,
    __out IMoniker** ppmk)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::GetContainer(__out IOleContainer** ppContainer)
{
    *ppContainer = NULL;
    return E_NOINTERFACE;
}

STDMETHODIMP LAxCtrl::ShowObject(void)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::OnShowWindow(__in BOOL fShow)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::RequestNewObjectLayout(void)
{
    return E_NOTIMPL;
}

// IOleInPlaceSite

STDMETHODIMP LAxCtrl::CanInPlaceActivate(void)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::OnInPlaceActivate(void)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::OnUIActivate(void)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::GetWindowContext(
    __out IOleInPlaceFrame** ppFrame,
    __out IOleInPlaceUIWindow** ppDoc,
    __out LPRECT lprcPosRect,
    __out LPRECT lprcClipRect,
    __inout LPOLEINPLACEFRAMEINFO lpFrameInfo)
{
    *ppFrame = (IOleInPlaceFrame*)this;
    *ppDoc = NULL;

    lpFrameInfo->fMDIApp = FALSE;
    lpFrameInfo->hwndFrame = m_hWnd;
    lpFrameInfo->cAccelEntries = 0;
    lpFrameInfo->haccel = NULL;

    return S_OK;
}

STDMETHODIMP LAxCtrl::Scroll(__in SIZE scrollExtant)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::OnUIDeactivate(__in BOOL fUndoable)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::OnInPlaceDeactivate(void)
{
    return S_OK;
}

STDMETHODIMP LAxCtrl::DiscardUndoState(void)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::DeactivateAndUndo(void)
{
    return E_NOTIMPL;
}

STDMETHODIMP LAxCtrl::OnPosRectChange(__in LPCRECT lprcPosRect)
{
    LComQIPtr<IOleInPlaceObject> pObjInPlace = m_pObject;
    if (pObjInPlace)
        pObjInPlace->SetObjectRects(lprcPosRect, lprcPosRect);
    return S_OK;
}

PDL_BEGIN_MSGMAP(LAxCtrl)
    PROCESS_SIZE(OnSize)
PDL_END_MSGMAP(LWindow)

void LAxCtrl::OnSize(UINT nType, int cx, int cy, BOOL& bHandled)
{
    if (NULL == m_pObject)
    {
        bHandled = FALSE;
        return;
    }

    RECT rc = { 0, 0, cx, cy };
    LComQIPtr<IOleInPlaceObject> pObjInPlace = m_pObject;
    if (pObjInPlace)
        pObjInPlace->SetObjectRects(&rc, &rc);
}
