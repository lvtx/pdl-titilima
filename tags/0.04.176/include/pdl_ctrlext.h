/**
 * \file pdl_ctrlext.h
 * \brief PDL ��չ�ؼ�
 * \details ����ļ��ж����˳��õķǱ�׼�ؼ������£�
 *   \li \c LHyperLink PDL ����������
 *   \li \c LSplitter PDL �ָ�������
 */

#pragma once

#include "pdl_base.h"
#include "pdl_window.h"
#include "pdl_message.h"
#include "pdl_ctrl.h"
#include "pdl_commctrl.h"
#include "pdl_gdi.h"

/**
 * \class LHyperLink
 * \brief PDL ����������
 */

class LHyperLink : public LStatic, protected LSubclassWnd
{
    PDL_DECLARE_WINCLASS(LHyperLink)
public:
    LHyperLink(void);
    LHyperLink& operator=(__in HWND hWnd);
public:

    /**
     * �� LHyperLink ��������һ�������ϡ�
     * @param [in] hWnd һ����Ч�Ĵ��ھ����
     */
    BOOL Attach(__in HWND hWnd);

    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in PVOID lpParam);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * ���������ͣ���ı�ɫ��
     * @param [in] clrHover �µ������ͣ�ı�ɫ��
     * @param [in] bRedraw �Ƿ��ػ洰�ڡ�
     * @return ԭ�е������ͣ�ı�ɫ��
     */
    COLORREF SetHoverColor(__in COLORREF clrHover, __in BOOL bRedraw = TRUE);

    /**
     * ������ͨ״̬���ı�ɫ��
     * @param [in] clrNormal �µ���ͨ״̬�ı�ɫ��
     * @param [in] bRedraw �Ƿ��ػ洰�ڡ�
     * @return ԭ�е���ͨ״̬�ı�ɫ��
     */
    COLORREF SetNormalColor(__in COLORREF clrNormal, __in BOOL bRedraw = TRUE);

protected:
    PDL_DECLARE_MSGMAP();
    DECLARE_ERASEBKGND_HANDLER(OnEraseBkgnd);
    DECLARE_MOUSELEAVE_HANDLER(OnMouseLeave);
    DECLARE_MOUSEMOVE_HANDLER(OnMouseMove);
    DECLARE_PAINT_HANDLER(OnPaint);
    DECLARE_SETCURSOR_HANDLER(OnSetCursor);
private:
    /**
     * ����Ƿ���ͣ
     */
    BOOL m_bHover;
    /**
     * ���ָ��
     */
    HCURSOR m_hCursor;
    /**
     * ��ͨ״̬�ı�ɫ
     */
    COLORREF m_clrNormal;
    /**
     * �����ͣ�ı�ɫ
     */
    COLORREF m_clrHover;
};

/**
 * \def LSPLIT_LEFTPANE
 * �ָ��������
 * \sa LSplitter::SetPane
 */
#define LSPLIT_LEFTPANE     0
/**
 * \def LSPLIT_TOPPANE
 * �ָ��������
 * \sa LSplitter::SetPane
 */
#define LSPLIT_TOPPANE      0
/**
 * \def LSPLIT_RIGHTPANE
 * �ָ��������
 * \sa LSplitter::SetPane
 */
#define LSPLIT_RIGHTPANE    1
/**
 * \def LSPLIT_BOTTOMPANE
 * �ָ��������
 * \sa LSplitter::SetPane
 */
#define LSPLIT_BOTTOMPANE   1

/**
 * \class LSplitter
 * \brief PDL �ָ�������
 */

class LSplitter : public LWindow
{
    PDL_DECLARE_WINCLASS(LSplitter)
public:
    /**
     * ���캯��
     * @param [in] bVertical �Ƿ�ֱ�ָ�����
     * @param [in] nSplitSize �ָ����ߴ硣
     * @param [in] bFullDrag �϶�ʱ�Ƿ�ʵʱ�ı�����С��
     */
    LSplitter(__in BOOL bVertical = TRUE, __in int nSplitSize = 3,
        __in BOOL bFullDrag = FALSE);
public:

    /**
     * �����ָ�����
     * @param [in] hParent �����ھ����
     * @param [in] nID ���ڿؼ� ID��
     * @param [in] lpRect �ָ�����ռ���Σ���Ϊ NULL �����������ڡ�
     * @return ��������ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in HWND hParent, __in UINT nID,
        __in LPCRECT lpRect = NULL);

    /**
     * �����ָ�����
     * @param [in] hParent �����ھ����
     * @param [in] nID ���ڿؼ� ID��
     * @param [in] x �ָ���λ�ú����ꡣ
     * @param [in] y �ָ���λ�������ꡣ
     * @param [in] cx �ָ�����ȡ�
     * @param [in] cy �ָ����߶ȡ�
     * @return ��������ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in HWND hParent, __in UINT nID, __in int x,
        __in int y, __in int cx, __in int cy);

    /**
     * ��÷ָ����ķָ�λ�á�
     * @return �ָ����ķָ�λ�á�
     */
    int GetSplitPos(void);

    /**
     * ����ȫ���϶����ԡ�
     * @param [in] bFullDrag �µ�ȫ���϶����ԡ�
     * @return ԭ�е�ȫ���϶����ԡ�
     */
    BOOL SetFullDrag(__in BOOL bFullDrag);

    /**
     * ���÷ָ�����塣
     * @param [in] i Ҫ���õ����ţ���ȡ����ֵ֮һ��
     *   \li \c LSPLIT_LEFTPANE
     *   \li \c LSPLIT_RIGHTPANE
     *   \li \c LSPLIT_TOPPANE
     *   \li \c LSPLIT_BOTTOMPANE
     * @param [in] hWnd Ҫ����Ϊ���Ĵ��ڡ�
     * @param [in] nSize �����Ĵ�С�����ȡ -1����ռ��ʣ��ռ䣨ֻ�Եڶ��������Ч����
     * @param [in] nMinSize ������С�ߴ硣
     * @return ������óɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SetPane(__in int i, __in HWND hWnd, __in int nSize = -1,
        __in int nMinSize = 5);

    /**
     * ���÷ָ����ķָ�λ�á�
     * @param [in] nPos �ָ����ķָ�λ�á�
     * @return ������óɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SetSplitPos(__in int nPos);

    /**
     * ���÷ָ�����С��
     * @param [in] nSplitSize �ָ������´�С��
     * @return �ָ�����ԭ�д�С��
     */
    int SetSplitSize(__in int nSplitSize = 3);

    /**
     * ������岼�֡�
     * @param [in] rcSplitter �ָ��������ھ��Ρ�
     */
    virtual void UpdateLayout(__in const RECT& rcSplitter);

protected:
    /**
     * ���Ʒָ�����
     */
    virtual void DrawSplitter(__in LDC *pDC, __in const RECT &rcSplitter);
protected:
    PDL_DECLARE_MSGMAP();
    DECLARE_COMMAND_HANDLER(OnCommand);
    DECLARE_LBUTTONDOWN_HANDLER(OnLButtonDown);
    DECLARE_LBUTTONUP_HANDLER(OnLButtonUp);
    DECLARE_MOUSEMOVE_HANDLER(OnMouseMove);
    DECLARE_SETCURSOR_HANDLER(OnSetCursor);
    DECLARE_SIZE_HANDLER(OnSize);
protected:
    /**
     * ���
     */
    LWnd m_wndPane[2];
    /**
     * �ָ�λ��
     */
    int m_nSplitPos;
    /**
     * �ָ����ߴ�
     */
    int m_nSplitSize;
    /**
     * ��һ��������С�ߴ�
     */
    int m_nMinSize1;
    /**
     * �ڶ���������С�ߴ�
     */
    int m_nMinSize2;
    /**
     * �Ƿ�ֱ�ָ���
     */
    BOOL m_bVertical;
    /**
     * �Ƿ�ȫ���϶�
     */
    BOOL m_bFullDrag;
    /**
     * �Ƿ�����˷ָ���
     */
    BOOL m_bBarDrawed;
};
