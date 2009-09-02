/**
 * \file pdl_container.h
 * \brief PDL 常用容器
 * \details 这个文件中实现了 PDL 中的常用容器：
 *   \li \c LPtrList PDL 链表类
 *   \li \c LPtrTree PDL 树类
 *   \li \c LPtrVector PDL 向量类
 *   \li \c LStrList PDL 字符串链表类
 */

#pragma once

#include "pdl_base.h"

/**
 * PDL 容器迭代器
 */
typedef struct { int unk; } *LIterator;

/**
 * 容器对象拷贝函数
 */
typedef void (*CopyPtr)(void* dst, const void* src);

/**
 * 容器对象销毁函数
 */
typedef void (*DestructPtr)(void* ptr);

/**
 * 迭代回调函数
 * @param [in] p 当前元素的指针。
 * @param [in] param 用户自定义参数。
 * @return 如果返回 TRUE 则继续迭代，否则停止迭代。
 * \sa LPtrList::ForEach
 * \sa LPtrVector::ForEach
 */
typedef BOOL (*IteratePtr)(void* p, void* param);

/**
 * 排序回调函数
 * @param [in] p1 第一个元素的指针。
 * @param [in] p2 第二个元素的指针。
 * @return 如果返回 TRUE 则交换两个元素的位置，否则不作处理。
 * \sa LPtrList::Sort
 * \sa LPtrVector::Sort
 */
typedef BOOL (*ComparePtr)(void* p1, void* p2);

/**
 * \class LPtrList
 * \brief PDL 链表类
 */

class LPtrList
{
public:
    LPtrList(void);
    ~LPtrList(void);
public:

    /**
     * 添加一个元素到链表头。
     * @param [in] ptr 来源元素的地址。
     * @return 如果添加成功则返回添加后的元素迭代器，否则返回 NULL。
     */
    LIterator AddHead(__in LPCVOID ptr);

    /**
     * 添加一个元素到链表尾。
     * @param [in] ptr 来源元素的地址。
     * @return 如果添加成功则返回添加后的元素迭代器，否则返回 NULL。
     */
    LIterator AddTail(__in LPCVOID ptr);

    /**
     * 清空链表。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Clear(void);

    /**
     * 创建链表。
     * @param [in] dwUnitSize 元素的大小。
     * @param [in] pfnCopy 元素的复制函数。
     * @param [in] pfnDestroy 元素的销毁函数。
     * @param [in] lock 操作锁。
     */
    void Create(__in DWORD dwUnitSize, __in CopyPtr pfnCopy = NULL,
        __in DestructPtr pfnDestroy = NULL, __in ILock* lock = NULL);

    /**
     * 销毁链表。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    void Destroy(void);

    /**
     * 迭代链表。
     * @param [in] pfnCallBack 迭代回调函数。
     * @param [in] param 自定义参数。
     * @return 如果迭代完毕则返回 NULL，否则返回迭代中止时的迭代器。
     * \note 在迭代过程中，请勿调用 SetAt 之外的任何修改操作。
     * \sa IteratePtr
     */
    LIterator ForEach(__in IteratePtr pfnCallBack, __in PVOID param);

    /**
     * 获取指定位置的元素。
     * @param [in] it 一个有效位置的迭代器。
     * @param [out] p 用于接收元素数据的缓冲区指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetAt(__in LIterator it, __out PVOID p);

    /**
     * 获取元素个数。
     * @return 链表元素的个数。
     */
    DWORD GetCount(void);

    /**
     * 获取链表头部的迭代器。
     * @return 链表头部的迭代器。
     */
    LIterator GetHeadIterator(void);

    /**
     * 获取指定迭代器的下一个迭代器。
     * @param [in] it 一个有效的迭代器。
     * @return 如果成功则返回指定迭代器的下一个迭代器，否则返回 NULL。
     */
    LIterator GetNextIterator(__in LIterator it);

    /**
     * 获取指定迭代器的前一个迭代器。
     * @param [in] it 一个有效的迭代器。
     * @return 如果成功则返回指定迭代器的前一个迭代器，否则返回 NULL。
     */
    LIterator GetPrevIterator(__in LIterator it);

    /**
     * 获取链表尾部的迭代器。
     * @return 链表尾部的迭代器。
     */
    LIterator GetTailIterator(void);

    /**
     * 向指定位置的后面插入一个元素。
     * @param [in] it 要插入数据的位置。
     * @param [in] ptr 要插入的数据。
     * @return 如果插入成功则返回插入后的位置，否则返回 NULL。
     */
    LIterator InsertAfter(__in LIterator it, __in LPCVOID ptr);

    /**
     * 向指定位置的前面插入一个元素。
     * @param [in] it 要插入数据的位置。
     * @param [in] ptr 要插入的数据。
     * @return 如果插入成功则返回插入后的位置，否则返回 NULL。
     */
    LIterator InsertBefore(__in LIterator it, __in LPCVOID ptr);

    /**
     * 设置指定位置的数据。
     * @param [in] it 要设置新数据的位置。
     * @param [in] ptr 要设置的新数据。
     */
    void SetAt(__in LIterator it, __in LPCVOID ptr);

    /**
     * 排序。
     * @param [in] pfnCompare 元素比较回调，当回调返回 TRUE 时则交换两个元素。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Sort(__in ComparePtr pfnCompare);

    /**
     * 移除指定位置的数据。
     * @param [in] it 要移除的位置。
     * @return 如果移除成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Remove(__in LIterator it);

protected:
    /**
     * 获得可用的操作锁。
     */
    PDLINLINE ILock* GetSafeLock(void) const;
    /**
     * 为新元素申请空间。
     */
    LIterator New(__in LPCVOID ptr);
protected:
    /**
     * 链表状态
     */
    DWORD m_dwStatus;
    /**
     * 链表头结点
     */
    LIterator m_itHead;
    /**
     * 链表尾结点
     */
    LIterator m_itTail;
    /**
     * 元素的大小
     */
    DWORD m_dwUnitSize;
    /**
     * 元素复制函数
     */
    CopyPtr m_pfnCopy;
    /**
     * 元素销毁函数
     */
    DestructPtr m_pfnDestroy;
    /**
     * 操作锁
     */
    ILock* m_lock;
};

#define LT_FIRST    1
#define LT_LAST     2
#define LT_ROOT     ((LIterator)NULL)

/**
 * \class LPtrTree
 * \brief PDL 树类
 */

class LPtrTree
{
public:
    LPtrTree(void);
    ~LPtrTree(void);
public:

    /**
     * 添加一个元素。
     * @param [in] it 要添加元素的父结点，如为 LT_ROOT 则添加根结点。
     * @param [in] ptr 来源元素的地址。
     * @param [in] type 要添加子结点的顺序：
     *   \li \c LT_FIRST 添加为第一个子结点。
     *   \li \c LT_LAST 添加为最后一个子结点。
     * @return 如果添加成功则返回添加后的元素迭代器，否则返回 NULL。
     */
    LIterator AddChild(__in LIterator it, __in LPCVOID ptr, __in DWORD type);

    /**
     * 清空树。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Clear(void);

    /**
     * 创建树。
     * @param [in] dwUnitSize 元素的大小。
     * @param [in] pfnCopy 元素的复制函数。
     * @param [in] pfnDestroy 元素的销毁函数。
     * @param [in] lock 操作锁。
     */
    void Create(__in DWORD dwUnitSize, __in CopyPtr pfnCopy = NULL,
        __in DestructPtr pfnDestroy = NULL, __in ILock* lock = NULL);

    /**
     * 销毁树。
     */
    void Destroy(void);

    /**
     * 获取指定位置的元素。
     * @param [in] it 一个有效位置的迭代器。
     * @param [out] p 用于接收元素数据的缓冲区指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetAt(__in LIterator it, __out PVOID p);

    /**
     * 获取指定结点的子结点。
     * @param [in] it 指定的父结点，使用 LT_ROOT 可以获得根结点。
     * @param [in] type 要获得子结点的类型：
     *   \li \c LT_FIRST 获取第一个子结点。
     *   \li \c LT_LAST 获取最后一个子结点。
     * @return 如果成功则返回指定的子结点，否则返回 NULL。
     */
    LIterator GetChild(__in LIterator it, __in DWORD type);

    /**
     * 获取指定结点的下一个兄弟结点。
     * @param [in] it 一个有效的迭代器。
     * @return 如果成功则返回指定迭代器的下一个兄弟结点，否则返回 NULL。
     */
    LIterator GetNextSibling(__in LIterator it);

    /**
     * 获取指定结点的前一个兄弟结点。
     * @param [in] it 一个有效的迭代器。
     * @return 如果成功则返回指定迭代器的前一个兄弟结点，否则返回 NULL。
     */
    LIterator GetPrevSibling(__in LIterator it);

    /**
     * 向指定位置的后面插入一个元素。
     * @param [in] it 要插入数据的位置。
     * @param [in] ptr 要插入的数据。
     * @return 如果插入成功则返回插入后的位置，否则返回 NULL。
     */
    LIterator InsertAfter(__in LIterator it, __in LPCVOID ptr);

    /**
     * 向指定位置的前面插入一个元素。
     * @param [in] it 要插入数据的位置。
     * @param [in] ptr 要插入的数据。
     * @return 如果插入成功则返回插入后的位置，否则返回 NULL。
     */
    LIterator InsertBefore(__in LIterator it, __in LPCVOID ptr);

    /**
     * 设置指定位置的数据。
     * @param [in] it 要设置新数据的位置。
     * @param [in] ptr 要设置的新数据。
     */
    void SetAt(__in LIterator it, __in LPCVOID ptr);

    /**
     * 移除指定位置的数据。
     * @param [in] it 要移除的位置。
     * @return 如果移除成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Remove(__in LIterator it);

protected:
    /**
     * 获得可用的操作锁。
     */
    PDLINLINE ILock* GetSafeLock(void) const;
    /**
     * 为新元素申请空间。
     */
    LIterator New(__in LPCVOID ptr);
protected:
    /**
     * 状态
     */
    DWORD m_dwStatus;
    /**
     * 第一个根结点
     */
    LIterator m_itRootFirst;
    /**
     * 最后一个根结点
     */
    LIterator m_itRootLast;
    /**
     * 元素的大小
     */
    DWORD m_dwUnitSize;
    /**
     * 元素复制函数
     */
    CopyPtr m_pfnCopy;
    /**
     * 元素销毁函数
     */
    DestructPtr m_pfnDestroy;
    /**
     * 操作锁
     */
    ILock* m_lock;
};

#define LV_FIRST    0
#define LV_LAST     (-1)

/**
 * \class LPtrVector
 * \brief PDL 向量类
 */

class LPtrVector
{
public:
    LPtrVector(void);
    ~LPtrVector(void);
public:

    /**
     * 清空向量。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Clear(void);

    /**
     * 创建向量。
     * @param [in] dwUnitSize 元素的大小。
     * @param [in] dwMaxCnt 初始化的向量大小。
     * @param [in] nGrowCnt 当向量扩容时增加的元素数，-1 表示容量加倍。
     * @param [in] pfnCopy 元素的复制函数。
     * @param [in] pfnDestroy 元素的销毁函数。
     * @param [in] lock 操作锁。
     */
    BOOL Create(__in DWORD dwUnitSize, __in DWORD dwMaxCnt,
        __in int nGrowCnt = -1, __in CopyPtr pfnCopy = NULL,
        __in DestructPtr pfnDestroy = NULL, __in ILock* lock = NULL);

    /**
     * 销毁向量。
     */
    void Destroy(void);

    /**
     * 迭代向量。
     * @param [in] pfnCallBack 迭代回调函数。
     * @param [in] param 自定义参数。
     * @return 如果迭代完毕则返回 -1，否则返回迭代中止时的元素索引。
     * \note 在迭代过程中，请勿调用 SetAt 之外的任何修改操作。
     * \sa IteratePtr
     */
    int ForEach(__in IteratePtr pfnCallBack, __in PVOID param);

    /**
     * 获取指定位置的数据。
     * @param [in] idx 要获取数据的索引位置。
     * @param [out] buf 用于接收数据的缓冲区指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetAt(__in int idx, __out PVOID buf);

    /**
     * 获取当前元素的个数。
     * @return 当前元素的个数。
     */
    DWORD GetCount(void);

    /**
     * 向指定位置的后面插入一个元素。
     * @param [in] idx 要插入数据的索引。
     * @param [in] ptr 要插入的数据。
     * @return 如果插入成功则返回插入后的索引，否则返回 -1。
     */
    int InsertAfter(__in int idx, __in LPCVOID pvData);

    /**
     * 向指定位置的前面插入一个元素。
     * @param [in] idx 要插入数据的索引。
     * @param [in] ptr 要插入的数据。
     * @return 如果插入成功则返回插入后的索引，否则返回 -1。
     */
    int InsertBefore(__in int idx, __in LPCVOID pvData);

    /**
     * 移除指定位置的数据。
     * @param [in] idx 要移除的位置。
     * @return 如果移除成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Remove(__in int idx);

    /**
     * 设置指定位置的数据。
     * @param [in] idx 要设置新数据的索引。
     * @param [in] ptr 要设置的新数据。
     */
    BOOL SetAt(__in int idx, __in LPCVOID pvData);

    /**
     * 排序。
     * @param [in] pfnCompare 元素比较回调，当回调返回 TRUE 时则交换两个元素。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Sort(__in ComparePtr pfnCompare);
protected:
    /**
     * 获得指定索引对应的元素地址。
     */
    PDLINLINE PVOID DataFromPos(__in int idx);
    /**
     * 获得可用的操作锁。
     */
    PDLINLINE ILock* GetSafeLock(void) const;
    /**
     * 增长向量的大小。
     */
    void Grow(void);
protected:
    /**
     * 为新元素申请空间。
     */
    LIterator New(__in LPCVOID ptr);
protected:
    /**
     * 向量状态
     */
    DWORD m_dwStatus;
    /**
     * 向量数据
     */
    PVOID m_pvData;
    /**
     * 元素大小
     */
    DWORD m_dwUnitSize;
    /**
     * 元素个数
     */
    DWORD m_dwUnitCnt;
    /**
     * 向量数据最大个数
     */
    DWORD m_dwMaxCnt;
    /**
     * 增长数量
     */
    int m_nGrowCnt;
    /**
     * 元素复制函数
     */
    CopyPtr m_pfnCopy;
    /**
     * 元素销毁函数
     */
    DestructPtr m_pfnDestroy;
    /**
     * 操作锁
     */
    ILock* m_lock;
};

/**
 * \class LStrList
 * \brief PDL 字符串链表类
 */

/**
 * 是否清空原有数据
 */
#define SLFILE_CLEAR        0x00000001
/**
 * 是否包含空行
 */
#define SLFILE_INCLUDENULL  0x00000002

class LStringA;
class LStringW;
class LStrList : protected LPtrList
{
public:

    /**
     * 构造函数。
     * @param [in] lock 操作锁。
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
     * 从一个指定的文本文件中加载一个字符串链表。
     * @param [in] lpFile 要加载的文本文件名。
     * @param [in] dwFlags 加载的方式：
     *   \li \c SLFILE_CLEAR 是否清空原有数据。
     *   \li \c SLFILE_INCLUDENULL 是否包含空行。
     * @return 加载的字符串数。
     * \note 每个字符串是以换行分隔的。
     */
    DWORD LoadFromFile(__in PCSTR lpFile, __in DWORD dwFlags);

    /**
     * 从一个指定的文本文件中加载一个字符串链表。
     * @param [in] lpFile 要加载的文本文件名。
     * @param [in] dwFlags 加载的方式：
     *   \li \c SLFILE_CLEAR 是否清空原有数据。
     *   \li \c SLFILE_INCLUDENULL 是否包含空行。
     * @return 加载的字符串数。
     * \note 每个字符串是以换行分隔的。
     */
    DWORD LoadFromFile(__in PCWSTR lpFile, __in DWORD dwFlags);

    /**
     * 将链表内容保存到一个文本文件中。
     * @param [in] lpFile 要保存的文本文件名。
     * @param [in] dwFlags 保存的方式：
     *   \li \c SLFILE_CLEAR 是否清空文件的原有数据。
     *   \li \c SLFILE_INCLUDENULL 是否包含空行。
     * @return 加载的字符串数。
     * \note 保存后每个字符串是以换行分隔的。
     */
    DWORD SaveToFile(__in PCSTR lpFile, __in DWORD dwFlags);

    /**
     * 将链表内容保存到一个文本文件中。
     * @param [in] lpFile 要保存的文本文件名。
     * @param [in] dwFlags 保存的方式：
     *   \li \c SLFILE_CLEAR 是否清空文件的原有数据。
     *   \li \c SLFILE_INCLUDENULL 是否包含空行。
     * @return 加载的字符串数。
     * \note 保存后每个字符串是以换行分隔的。
     */
    DWORD SaveToFile(__in PCWSTR lpFile, __in DWORD dwFlags);

    void SetAt(__in LIterator it, __in PCSTR lpString);
    void SetAt(__in LIterator it, __in PCWSTR lpString);
protected:
    PCSTR GetAt(__in LIterator it);
};
