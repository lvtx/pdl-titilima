/**
 * \file pdl_container.h
 * \brief PDL ��������
 * \details ����ļ���ʵ���� PDL �еĳ���������
 *   \li \c LPtrList PDL ������
 *   \li \c LPtrVector PDL ������
 *   \li \c LStrListA PDL Ansi �ַ���������
 */

#pragma once

#include <pdl_base.h>

/**
 * PDL ����������
 */
typedef void *LIterator;

/**
 * �������󿽱�����
 */
typedef void (*CopyPtr)(void* dst, const void* src);

/**
 * �����������ٺ���
 */
typedef void (*DestructPtr)(void* ptr);

/**
 * \class LPtrList
 * \brief PDL ������
 */

class LPtrList
{
public:
    LPtrList(void);
    ~LPtrList(void);

    /**
     * �����ص�����
     * @param [in] This ����ָ�롣
     * @param [in] it ��ǰ��������
     * @param [in] param �û��Զ��������
     * @return ������� TRUE ���������������ֹͣ������
     * \sa ForEach
     */
    typedef BOOL (*IteratePtr)(LPtrList* This, LIterator it, void* param);

public:

    /**
     * ���һ��Ԫ�ص�����ͷ��
     * @param [in] ptr ��ԴԪ�صĵ�ַ��
     * @return �����ӳɹ��򷵻���Ӻ��Ԫ�ص�ַ�����򷵻� NULL��
     */
    PVOID AddHead(__in LPCVOID ptr);

    /**
     * ���һ��Ԫ�ص�����β��
     * @param [in] ptr ��ԴԪ�صĵ�ַ��
     * @return �����ӳɹ��򷵻���Ӻ��Ԫ�ص�ַ�����򷵻� NULL��
     */
    PVOID AddTail(__in LPCVOID ptr);

    /**
     * �������
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Clear(void);

    /**
     * ��������
     * @param [in] dwUnitSize Ԫ�صĴ�С��
     * @param [in] pfnCopy Ԫ�صĸ��ƺ�����
     * @param [in] pfnDestroy Ԫ�ص����ٺ�����
     * @param [in] lock ��������
     */
    void Create(__in DWORD dwUnitSize, __in CopyPtr pfnCopy = NULL,
        __in DestructPtr pfnDestroy = NULL, __in ILock* lock = NULL);

    /**
     * ��������
     */
    void Destroy(void);

    /**
     * ��������
     * @param [in] pfnCallBack �����ص�������
     * @param [in] param �Զ��������
     * @return �����������򷵻� NULL�����򷵻ص�����ֹʱ�ĵ�������
     * \note �ڵ��������У�������� SetAt ֮����κ��޸Ĳ�����
     * \sa IteratePtr
     */
    LIterator ForEach(__in IteratePtr pfnCallBack, __in PVOID param);

    /**
     * ��ȡָ��λ�õ�Ԫ�ء�
     * @param [in] it һ����Чλ�õĵ�������
     * @param [out] p ���ڽ���Ԫ�����ݵĻ�����ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetAt(__in LIterator it, __out PVOID p);

    /**
     * ��ȡԪ�ظ�����
     * @return ����Ԫ�صĸ�����
     */
    DWORD GetCount(void);

    /**
     * ��ȡ����ͷ���ĵ�������
     * @return ����ͷ���ĵ�������
     */
    LIterator GetHeadIterator(void);

    /**
     * ��ȡָ������������һ����������
     * @param [in, out] it ����һ��ָ���ĵ���������������һ����������
     */
    void GetNextIterator(__inout LIterator* it);

    /**
     * ��ȡָ����������ǰһ����������
     * @param [in, out] it ����һ��ָ���ĵ�������������ǰһ����������
     */
    void GetPrevIterator(__inout LIterator* it);

    /**
     * ��ȡ����β���ĵ�������
     * @return ����β���ĵ�������
     */
    LIterator GetTailIterator(void);

    /**
     * ��ָ��λ�õĺ������һ��Ԫ�ء�
     * @param [in] it Ҫ�������ݵ�λ�á�
     * @param [in] ptr Ҫ��������ݡ�
     * @return �������ɹ��򷵻ز�����λ�ã����򷵻� NULL��
     */
    LIterator InsertAfter(__in LIterator it, __in LPCVOID ptr);

    /**
     * ��ָ��λ�õ�ǰ�����һ��Ԫ�ء�
     * @param [in] it Ҫ�������ݵ�λ�á�
     * @param [in] ptr Ҫ��������ݡ�
     * @return �������ɹ��򷵻ز�����λ�ã����򷵻� NULL��
     */
    LIterator InsertBefore(__in LIterator it, __in LPCVOID ptr);

    /**
     * ����ָ��λ�õ����ݡ�
     * @param [in] it Ҫ���������ݵ�λ�á�
     * @param [in] ptr Ҫ���õ������ݡ�
     */
    void SetAt(__in LIterator it, __in LPCVOID ptr);

    /**
     * �Ƴ�ָ��λ�õ����ݡ�
     * @param [in] it Ҫ�Ƴ���λ�á�
     * @return ����Ƴ��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Remove(__in LIterator it);

protected:
    /**
     * ��ÿ��õĲ�������
     */
    PDLINLINE ILock* GetSafeLock(void) const;
    /**
     * Ϊ��Ԫ������ռ䡣
     */
    LIterator New(__in LPCVOID ptr);
protected:
    /**
     * ����״̬
     */
    DWORD m_dwStatus;
    /**
     * ����ͷ���
     */
    LIterator m_itHead;
    /**
     * ����β���
     */
    LIterator m_itTail;
    /**
     * Ԫ�صĴ�С
     */
    DWORD m_dwUnitSize;
    /**
     * Ԫ�ظ��ƺ���
     */
    CopyPtr m_pfnCopy;
    /**
     * Ԫ�����ٺ���
     */
    DestructPtr m_pfnDestroy;
    /**
     * ������
     */
    ILock* m_lock;
};

/**
 * \class LPtrVector
 * \brief PDL ������
 */

class LPtrVector
{
public:
    LPtrVector(void);
    ~LPtrVector(void);

    /**
     * �����ص�����
     * @param [in] This ����ָ�롣
     * @param [in] idx ��ǰ������Ԫ��������
     * @param [in] param �û��Զ��������
     * @return ������� TRUE ���������������ֹͣ������
     * \sa ForEach
     */
    typedef BOOL (*IteratePtr)(LPtrVector* This, int idx, void* param);

public:

    /**
     * ���������
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Clear(void);

    /**
     * ����������
     * @param [in] dwUnitSize Ԫ�صĴ�С��
     * @param [in] dwMaxCnt ��ʼ����������С��
     * @param [in] nGrowCnt ����������ʱ���ӵ�Ԫ������-1 ��ʾ�����ӱ���
     * @param [in] pfnCopy Ԫ�صĸ��ƺ�����
     * @param [in] pfnDestroy Ԫ�ص����ٺ�����
     * @param [in] lock ��������
     */
    BOOL Create(__in DWORD dwUnitSize, __in DWORD dwMaxCnt,
        __in int nGrowCnt = -1, __in CopyPtr pfnCopy = NULL,
        __in DestructPtr pfnDestroy = NULL, __in ILock* lock = NULL);

    /**
     * ����������
     */
    void Destroy(void);

    /**
     * ����������
     * @param [in] pfnCallBack �����ص�������
     * @param [in] param �Զ��������
     * @return �����������򷵻� -1�����򷵻ص�����ֹʱ��Ԫ��������
     * \note �ڵ��������У�������� SetAt ֮����κ��޸Ĳ�����
     * \sa IteratePtr
     */
    int ForEach(__in IteratePtr pfnCallBack, __in PVOID param);

    /**
     * ��ȡָ��λ�õ����ݡ�
     * @param [in] idx Ҫ��ȡ���ݵ�����λ�á�
     * @param [out] buf ���ڽ������ݵĻ�����ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetAt(__in int idx, __out PVOID buf);

    /**
     * ��ȡ��ǰԪ�صĸ�����
     * @return ��ǰԪ�صĸ�����
     */
    DWORD GetCount(void);

    /**
     * ��ָ��λ�õĺ������һ��Ԫ�ء�
     * @param [in] idx Ҫ�������ݵ�������
     * @param [in] ptr Ҫ��������ݡ�
     * @return �������ɹ��򷵻ز��������������򷵻� -1��
     */
    int InsertAfter(__in int idx, __in LPCVOID pvData);

    /**
     * ��ָ��λ�õ�ǰ�����һ��Ԫ�ء�
     * @param [in] idx Ҫ�������ݵ�������
     * @param [in] ptr Ҫ��������ݡ�
     * @return �������ɹ��򷵻ز��������������򷵻� -1��
     */
    int InsertBefore(__in int idx, __in LPCVOID pvData);

    /**
     * ����ָ��λ�õ����ݡ�
     * @param [in] idx Ҫ���������ݵ�������
     * @param [in] ptr Ҫ���õ������ݡ�
     */
    BOOL SetAt(__in int idx, __in LPCVOID pvData);
protected:
    /**
     * ���ָ��������Ӧ��Ԫ�ص�ַ��
     */
    PDLINLINE PVOID DataFromPos(__in int idx);
    /**
     * ��ÿ��õĲ�������
     */
    PDLINLINE ILock* GetSafeLock(void) const;
    /**
     * ���������Ĵ�С��
     */
    void Grow(void);
protected:
    /**
     * Ϊ��Ԫ������ռ䡣
     */
    LIterator New(__in LPCVOID ptr);
protected:
    /**
     * ����״̬
     */
    DWORD m_dwStatus;
    /**
     * ��������
     */
    PVOID m_pvData;
    /**
     * Ԫ�ش�С
     */
    DWORD m_dwUnitSize;
    /**
     * Ԫ�ظ���
     */
    DWORD m_dwUnitCnt;
    /**
     * ��������������
     */
    DWORD m_dwMaxCnt;
    /**
     * ��������
     */
    int m_nGrowCnt;
    /**
     * Ԫ�ظ��ƺ���
     */
    CopyPtr m_pfnCopy;
    /**
     * Ԫ�����ٺ���
     */
    DestructPtr m_pfnDestroy;
    /**
     * ������
     */
    ILock* m_lock;
};

/**
 * \class LStrListA
 * \brief PDL Ansi �ַ���������
 */

/**
 * �Ƿ����ԭ������
 */
#define SLFILE_CLEAR        0x00000001
/**
 * �Ƿ��������
 */
#define SLFILE_INCLUDENULL  0x00000002

class LStrListA : protected LPtrList
{
public:

    /**
     * ���캯����
     * @param [in] lock ��������
     */
    LStrListA(__in ILock* lock = NULL);

public:
    PCSTR AddHead(__in PCSTR lpString);
    PCSTR AddTail(__in PCSTR lpString);
    PCSTR GetAt(__in LIterator it);
    LIterator GetHeadIterator(void);
    void GetNextIterator(__inout LIterator* it);
    void GetPrevIterator(__inout LIterator* it);
    LIterator GetTailIterator(void);
    PCSTR InsertAfter(__in LIterator it, __in PCSTR lpString);
    PCSTR InsertBefore(__in LIterator it, __in PCSTR lpString);

    /**
     * ��һ��ָ�����ı��ļ��м���һ���ַ�������
     * @param [in] lpFile Ҫ���ص��ı��ļ�����
     * @param [in] dwFlags ���صķ�ʽ��
     *   \li \c SLFILE_CLEAR �Ƿ����ԭ�����ݡ�
     *   \li \c SLFILE_INCLUDENULL �Ƿ�������С�
     * @return ���ص��ַ�������
     * \note ÿ���ַ������Ի��зָ��ġ�
     */
    DWORD LoadFromFile(__in PCSTR lpFile, __in DWORD dwFlags);

    /**
     * ��һ��ָ�����ı��ļ��м���һ���ַ�������
     * @param [in] lpFile Ҫ���ص��ı��ļ�����
     * @param [in] dwFlags ���صķ�ʽ��
     *   \li \c SLFILE_CLEAR �Ƿ����ԭ�����ݡ�
     *   \li \c SLFILE_INCLUDENULL �Ƿ�������С�
     * @return ���ص��ַ�������
     * \note ÿ���ַ������Ի��зָ��ġ�
     */
    DWORD LoadFromFile(__in PCWSTR lpFile, __in DWORD dwFlags);

    /**
     * ���������ݱ��浽һ���ı��ļ��С�
     * @param [in] lpFile Ҫ������ı��ļ�����
     * @param [in] dwFlags ����ķ�ʽ��
     *   \li \c SLFILE_CLEAR �Ƿ�����ļ���ԭ�����ݡ�
     *   \li \c SLFILE_INCLUDENULL �Ƿ�������С�
     * @return ���ص��ַ�������
     * \note �����ÿ���ַ������Ի��зָ��ġ�
     */
    DWORD SaveToFile(__in PCSTR lpFile, __in DWORD dwFlags);

    /**
     * ���������ݱ��浽һ���ı��ļ��С�
     * @param [in] lpFile Ҫ������ı��ļ�����
     * @param [in] dwFlags ����ķ�ʽ��
     *   \li \c SLFILE_CLEAR �Ƿ�����ļ���ԭ�����ݡ�
     *   \li \c SLFILE_INCLUDENULL �Ƿ�������С�
     * @return ���ص��ַ�������
     * \note �����ÿ���ַ������Ի��зָ��ġ�
     */
    DWORD SaveToFile(__in PCWSTR lpFile, __in DWORD dwFlags);

    void SetAt(__in LIterator it, __in PCSTR lpString);
};
