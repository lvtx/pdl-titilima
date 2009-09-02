/**
 * \file pdl_container.h
 * \brief PDL ��������
 * \details ����ļ���ʵ���� PDL �еĳ���������
 *   \li \c LPtrList PDL ������
 *   \li \c LPtrTree PDL ����
 *   \li \c LPtrVector PDL ������
 *   \li \c LStrList PDL �ַ���������
 */

#pragma once

#include "pdl_base.h"

/**
 * PDL ����������
 */
typedef struct { int unk; } *LIterator;

/**
 * �������󿽱�����
 */
typedef void (*CopyPtr)(void* dst, const void* src);

/**
 * �����������ٺ���
 */
typedef void (*DestructPtr)(void* ptr);

/**
 * �����ص�����
 * @param [in] p ��ǰԪ�ص�ָ�롣
 * @param [in] param �û��Զ��������
 * @return ������� TRUE ���������������ֹͣ������
 * \sa LPtrList::ForEach
 * \sa LPtrVector::ForEach
 */
typedef BOOL (*IteratePtr)(void* p, void* param);

/**
 * ����ص�����
 * @param [in] p1 ��һ��Ԫ�ص�ָ�롣
 * @param [in] p2 �ڶ���Ԫ�ص�ָ�롣
 * @return ������� TRUE �򽻻�����Ԫ�ص�λ�ã�����������
 * \sa LPtrList::Sort
 * \sa LPtrVector::Sort
 */
typedef BOOL (*ComparePtr)(void* p1, void* p2);

/**
 * \class LPtrList
 * \brief PDL ������
 */

class LPtrList
{
public:
    LPtrList(void);
    ~LPtrList(void);
public:

    /**
     * ���һ��Ԫ�ص�����ͷ��
     * @param [in] ptr ��ԴԪ�صĵ�ַ��
     * @return �����ӳɹ��򷵻���Ӻ��Ԫ�ص����������򷵻� NULL��
     */
    LIterator AddHead(__in LPCVOID ptr);

    /**
     * ���һ��Ԫ�ص�����β��
     * @param [in] ptr ��ԴԪ�صĵ�ַ��
     * @return �����ӳɹ��򷵻���Ӻ��Ԫ�ص����������򷵻� NULL��
     */
    LIterator AddTail(__in LPCVOID ptr);

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
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
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
     * @param [in] it һ����Ч�ĵ�������
     * @return ����ɹ��򷵻�ָ������������һ�������������򷵻� NULL��
     */
    LIterator GetNextIterator(__in LIterator it);

    /**
     * ��ȡָ����������ǰһ����������
     * @param [in] it һ����Ч�ĵ�������
     * @return ����ɹ��򷵻�ָ����������ǰһ�������������򷵻� NULL��
     */
    LIterator GetPrevIterator(__in LIterator it);

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
     * ����
     * @param [in] pfnCompare Ԫ�رȽϻص������ص����� TRUE ʱ�򽻻�����Ԫ�ء�
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Sort(__in ComparePtr pfnCompare);

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

#define LT_FIRST    1
#define LT_LAST     2
#define LT_ROOT     ((LIterator)NULL)

/**
 * \class LPtrTree
 * \brief PDL ����
 */

class LPtrTree
{
public:
    LPtrTree(void);
    ~LPtrTree(void);
public:

    /**
     * ���һ��Ԫ�ء�
     * @param [in] it Ҫ���Ԫ�صĸ���㣬��Ϊ LT_ROOT ����Ӹ���㡣
     * @param [in] ptr ��ԴԪ�صĵ�ַ��
     * @param [in] type Ҫ����ӽ���˳��
     *   \li \c LT_FIRST ���Ϊ��һ���ӽ�㡣
     *   \li \c LT_LAST ���Ϊ���һ���ӽ�㡣
     * @return �����ӳɹ��򷵻���Ӻ��Ԫ�ص����������򷵻� NULL��
     */
    LIterator AddChild(__in LIterator it, __in LPCVOID ptr, __in DWORD type);

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
     * ��ȡָ��λ�õ�Ԫ�ء�
     * @param [in] it һ����Чλ�õĵ�������
     * @param [out] p ���ڽ���Ԫ�����ݵĻ�����ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetAt(__in LIterator it, __out PVOID p);

    /**
     * ��ȡָ�������ӽ�㡣
     * @param [in] it ָ���ĸ���㣬ʹ�� LT_ROOT ���Ի�ø���㡣
     * @param [in] type Ҫ����ӽ������ͣ�
     *   \li \c LT_FIRST ��ȡ��һ���ӽ�㡣
     *   \li \c LT_LAST ��ȡ���һ���ӽ�㡣
     * @return ����ɹ��򷵻�ָ�����ӽ�㣬���򷵻� NULL��
     */
    LIterator GetChild(__in LIterator it, __in DWORD type);

    /**
     * ��ȡָ��������һ���ֵܽ�㡣
     * @param [in] it һ����Ч�ĵ�������
     * @return ����ɹ��򷵻�ָ������������һ���ֵܽ�㣬���򷵻� NULL��
     */
    LIterator GetNextSibling(__in LIterator it);

    /**
     * ��ȡָ������ǰһ���ֵܽ�㡣
     * @param [in] it һ����Ч�ĵ�������
     * @return ����ɹ��򷵻�ָ����������ǰһ���ֵܽ�㣬���򷵻� NULL��
     */
    LIterator GetPrevSibling(__in LIterator it);

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
     * ״̬
     */
    DWORD m_dwStatus;
    /**
     * ��һ�������
     */
    LIterator m_itRootFirst;
    /**
     * ���һ�������
     */
    LIterator m_itRootLast;
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

#define LV_FIRST    0
#define LV_LAST     (-1)

/**
 * \class LPtrVector
 * \brief PDL ������
 */

class LPtrVector
{
public:
    LPtrVector(void);
    ~LPtrVector(void);
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
     * �Ƴ�ָ��λ�õ����ݡ�
     * @param [in] idx Ҫ�Ƴ���λ�á�
     * @return ����Ƴ��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Remove(__in int idx);

    /**
     * ����ָ��λ�õ����ݡ�
     * @param [in] idx Ҫ���������ݵ�������
     * @param [in] ptr Ҫ���õ������ݡ�
     */
    BOOL SetAt(__in int idx, __in LPCVOID pvData);

    /**
     * ����
     * @param [in] pfnCompare Ԫ�رȽϻص������ص����� TRUE ʱ�򽻻�����Ԫ�ء�
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Sort(__in ComparePtr pfnCompare);
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
 * \class LStrList
 * \brief PDL �ַ���������
 */

/**
 * �Ƿ����ԭ������
 */
#define SLFILE_CLEAR        0x00000001
/**
 * �Ƿ��������
 */
#define SLFILE_INCLUDENULL  0x00000002

class LStringA;
class LStringW;
class LStrList : protected LPtrList
{
public:

    /**
     * ���캯����
     * @param [in] lock ��������
     */
    LStrList(__in ILock* lock = NULL);

public:
    LIterator AddHead(__in PCSTR lpString);
    LIterator AddHead(__in PCWSTR lpString);
    LIterator AddTail(__in PCSTR lpString);
    LIterator AddTail(__in PCWSTR lpString);
    BOOL GetAt(__in LIterator it, __out LStringA* str);
    BOOL GetAt(__in LIterator it, __out LStringW* str);
    LIterator GetHeadIterator(void);
    LIterator GetNextIterator(__in LIterator it);
    LIterator GetPrevIterator(__in LIterator it);
    LIterator GetTailIterator(void);
    LIterator InsertAfter(__in LIterator it, __in PCSTR lpString);
    LIterator InsertAfter(__in LIterator it, __in PCWSTR lpString);
    LIterator InsertBefore(__in LIterator it, __in PCWSTR lpString);
    LIterator InsertBefore(__in LIterator it, __in PCSTR lpString);

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
    void SetAt(__in LIterator it, __in PCWSTR lpString);
protected:
    PCSTR GetAt(__in LIterator it);
};
