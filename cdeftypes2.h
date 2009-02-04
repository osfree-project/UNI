#ifndef __CDEFTYPES2_H__
#define __CDEFTYPES2_H__
#define CDEFTYPES2_INCLUDED

/* types tricks */

typedef CHAR     * _Seg16 PCHAR16;
typedef UCHAR    * _Seg16 PUCHAR16;

typedef VOID     * _Seg16 PVOID16;


#ifndef LONGLONG_INCLUDED
#define LONGLONG_INCLUDED 1

#ifndef INCL_LONGLONG
typedef struct _LONGLONG {
    ULONG ulLo;
    LONG  ulHi;
} LONGLONG, *PLONGLONG;

typedef struct _ULONGLONG {
    ULONG ulLo;
    ULONG ulHi;
} ULONGLONG, *PULONGLONG;
#else
typedef long long LONGLONG, *PLONGLONG;
typedef unsigned long long ULONGLONG, *PULONGLONG;
#endif

#define MAKEULONG(l, h)  ((ULONG)(((USHORT)(l)) | ((ULONG)((USHORT)(h))) << 16))
#define MAKELONG(l, h)   ((LONG)MAKEULONG(l, h))
#define MAKEUSHORT(l, h) (((USHORT)(l)) | ((USHORT)(h)) << 8)
#define MAKESHORT(l, h)  ((SHORT)MAKEUSHORT(l, h))

#define MAKEP( sel,off )   ((void *)(void * _Seg16)( (sel) << 16 | (off) ))
#define MAKE16P( sel,off ) ((void * _Seg16)( (sel) << 16 | (off) ))

#define SELECTOROF(ptr)    ((((ULONG)(ptr))>>13)|7)
#define OFFSETOF(p)        (((PUSHORT)&(p))[0])

#define MAKETYPE(v, type)  (*((type *)&v))

#define FIELDOFFSET(type, field)   ((SHORT)&(((type *)0)->field))

#define LOBYTE(w)   LOUCHAR(w)
#define HIBYTE(w)   HIUCHAR(w)
#define LOUCHAR(w)  ((UCHAR)(w))
#define HIUCHAR(w)  ((UCHAR)(((USHORT)(w) >> 8) & 0xff))
#define LOUSHORT(l) ((USHORT)((ULONG)l))
#define HIUSHORT(l) ((USHORT)(((ULONG)(l) >> 16) & 0xffff))

#define MAKEERRORID(sev, error) (ERRORID)(MAKEULONG((error), (sev)))
#define ERRORIDERROR(errid)     (LOUSHORT(errid))
#define ERRORIDSEV(errid)       (HIUSHORT(errid))

#define APIENTRY16 _Far16 _Pascal
#define PASCAL16   _Far16 _Pascal
#define CDECL16    _Far16 _Cdecl


#endif

#endif
