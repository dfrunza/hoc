/*
 * MINICRT.H
 *
 * Copyright (c) 2014-2017 Malcolm J. Smith
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#define FILE minicrt_file
typedef struct _minicrt_file {
    HANDLE hFile;
} minicrt_file, *pminicrt_file;

/*
  Definitions taken from Win32
--------------------------------*/
#define malloc(x)   HeapAlloc(GetProcessHeap(), 0, x)
#define realloc(x,s) HeapReAlloc(GetProcessHeap(), 0, x, s)
#define free(x)     HeapFree(GetProcessHeap(), 0, x)
#define exit    ExitProcess
#define stdin  (FILE*)0
#define stdout (FILE*)1
#define stderr (FILE*)2
#define RAND_MAX 32768
#define INT_MAX (0x7fffffff)
#define UINT_MAX (0xffffffff)
/*--------------------------*/

int _fltused = 0;

void * memcpy(void * dest, const void * src, unsigned int len)
{
    unsigned int i;
    char * char_src = (char *)src;
    char * char_dest = (char *)dest;
    for (i = 0; i < len; i++) {
        char_dest[i] = char_src[i];
    }
    return dest;
}

int memcmp(const void * buf1, const void * buf2, unsigned int len)
{
    unsigned int i = 0;
    unsigned char * char_buf1 = (unsigned char *)buf1;
    unsigned char * char_buf2 = (unsigned char *)buf2;
    for (i = 0; i < len; i++) {
        if (char_buf1[i] < char_buf2[i]) {
            return -1;
        } else if (char_buf1[i] > char_buf2[i]) {
            return 1;
        }
    }
    return 0;
}

void * memmove(void * dest, const void * src, unsigned int len)
{
    unsigned int i;
    char * char_src = (char *)src;
    char * char_dest = (char *)dest;
    if (char_dest > char_src) {
        for (i = len; ; i--) {
            char_dest[i] = char_src[i];
            if (i==0) break;
        }
    } else {
        for (i = 0; i < len; i++) {
            char_dest[i] = char_src[i];
        }
    }
    return dest;
}

void * memset(void * dest, int ch, unsigned int len)
{
    unsigned char c = (unsigned char)ch;
    unsigned int i;
    unsigned int fill;
    unsigned int chunks = len / sizeof(fill);
    char * char_dest = (char *)dest;
    unsigned int * uint_dest = (unsigned int *)dest;

    //
    //  Note we go from the back to the front.  This is to
    //  prevent newer compilers from noticing what we're doing
    //  and trying to invoke the built-in memset instead of us.
    //

    fill = (c<<24) + (c<<16) + (c<<8) + c;

    for (i = len; i > chunks * sizeof(fill); i--) {
        char_dest[i - 1] = c;
    }

    for (i = chunks; i > 0; i--) {
        uint_dest[i - 1] = fill;
    }

    return dest;
}

int csncmp(const CHAR * str1, const CHAR * str2, unsigned int count)
{
    const CHAR * ptr1 = str1;
    const CHAR * ptr2 = str2;
    unsigned int remaining = count;

    while(TRUE) {
        if (*ptr1 < *ptr2) {
            return -1;
        } else if (*ptr1 > *ptr2) {
            return 1;
        } else if (*ptr1 == '\0') {
            return 0;
        }

        ptr1++;
        ptr2++;
        remaining--;

        if (remaining == 0) {
            return 0;
        }
    }
    return 0;
}

int cscmp(const CHAR * str1, const CHAR * str2)
{
    return csncmp(str1, str2, (unsigned int)-1);
}

#include "printf.c"
#define PRINTF_SIZEONLY 1
#include "printf.c"
#undef PRINTF_SIZEONLY

int
sprintf_s(LPTSTR szDest, unsigned int len, LPCTSTR szFmt, ...)
{
    va_list marker;
    int out_len;

    va_start( marker, szFmt );
    out_len = vsprintf_s(szDest, len, szFmt, marker);
    va_end( marker );
    return out_len;
}

#ifdef __MINGW32__
  #define sprintf   _sprintf
#endif
int
sprintf(LPTSTR szDest, LPCTSTR szFmt, ...)
{
    va_list marker;
    int out_len;

    va_start( marker, szFmt );
    out_len = vsprintf_s(szDest, (DWORD)-1, szFmt, marker);
    va_end( marker );
    return out_len;
}

int
vfprintf(FILE * fp, LPCTSTR szFmt, va_list marker)
{
    va_list savedmarker = marker;
    int len;
    CHAR stack_buf[20];
    HANDLE hOut;
    DWORD written;
    CHAR * buf;

    if (fp==stdin) {
        return 0;
    } else if (fp==stdout) {
        hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    } else if (fp==stderr) {
        hOut = GetStdHandle(STD_ERROR_HANDLE);
    } else {
        hOut = fp->hFile;
    }

    len = vsprintf_size(szFmt, marker);

    if (len>(int)(sizeof(stack_buf)/sizeof(stack_buf[0]))) {
        buf = HeapAlloc(GetProcessHeap(), 0, len * sizeof(CHAR));
        if (buf == NULL) {
            return 0;
        }
    } else {
        buf = stack_buf;
    }

    marker = savedmarker;
    len = vsprintf_s(buf, len, szFmt, marker);
    WriteFile(hOut,buf,len*sizeof(CHAR),&written,NULL);

    if (buf != stack_buf) {
        HeapFree(GetProcessHeap(), 0, buf);
    }
    return len;
}

#ifdef __MINGW32__
  #define fprintf   _fprintf
#endif
int
fprintf(FILE * fp, LPCTSTR szFmt, ...)
{
    va_list marker;
    int out_len;

    va_start( marker, szFmt );
    out_len = vfprintf(fp, szFmt, marker);
    va_end( marker );
    return out_len;
}

int
printf(LPCTSTR szFmt, ...)
{
    va_list marker;
    int out_len;

    va_start( marker, szFmt );
    out_len = vfprintf(stdout, szFmt, marker);
    va_end( marker );
    return out_len;
}

FILE * fopen(CHAR * filename, CHAR * mode)
{
    FILE * RetVal;
    DWORD DesiredAccess;
    DWORD CreateDisposition;

    if (cscmp(mode,"r")==0 ||
        cscmp(mode,"rb")==0) {

        DesiredAccess = GENERIC_READ;
        CreateDisposition = OPEN_EXISTING;

    } else if (cscmp(mode,"w")==0 ||
               cscmp(mode,"wb")==0) {

        DesiredAccess = GENERIC_WRITE;
        CreateDisposition = CREATE_ALWAYS;

    } else {
        return NULL;
    }

    RetVal = malloc(sizeof(FILE));
    if (RetVal == NULL) {
        return NULL;
    }

    RetVal->hFile = CreateFile(filename,
                               DesiredAccess,
                               FILE_SHARE_READ|FILE_SHARE_DELETE,
                               NULL,
                               CreateDisposition,
                               FILE_ATTRIBUTE_NORMAL,
                               NULL);

    if (RetVal->hFile != INVALID_HANDLE_VALUE) {
        return RetVal;
    } else {
        free(RetVal);
    }

    return NULL;
}

LPTSTR * tcmdlinetoargs(LPTSTR szCmdLine, int * argc)
{
    int arg_count = 0;
    int char_count = 0;
    CHAR break_char = ' ';
    CHAR * c;
    LPTSTR * ret;
    LPTSTR ret_str;

    //
    //  Consume all spaces.  After this, we're either at
    //  the end of string, or we have an arg, and it
    //  might start with a quote
    //

    c = szCmdLine;
    while (*c == ' ') c++;
    if (*c == '"') {
        break_char = '"';
        c++;
    }

    while (*c != '\0') {
        if (*c == break_char) {
            break_char = ' ';
            c++;
            while (*c == break_char) c++;
            if (*c == '"') {
                break_char = '"';
                c++;
            }
                arg_count++;
        } else {
            char_count++;

            //
            //  If we hit a break char, we count the argument then.
            //  If we hit end of string, count it here; note we're
            //  only counting it if we counted a character before it
            //  (ie., trailing whitespace is not an arg.)
            //

            c++;

            if (*c == '\0') {
                arg_count++;
            }
        }
    }

    *argc = arg_count;

    ret = HeapAlloc( GetProcessHeap(), 0, (arg_count * sizeof(LPTSTR)) + (char_count + arg_count) * sizeof(CHAR));

    ret_str = (LPTSTR)(ret + arg_count);

    arg_count = 0;
    ret[arg_count] = ret_str;

    //
    //  Consume all spaces.  After this, we're either at
    //  the end of string, or we have an arg, and it
    //  might start with a quote
    //

    c = szCmdLine;
    while (*c == ' ') c++;
    if (*c == '"') {
        break_char = '"';
        c++;
    }

    while (*c != '\0') {
        if (*c == break_char) {
            *ret_str = '\0';
            ret_str++;

            break_char = ' ';
            c++;
            while (*c == break_char) c++;
            if (*c == '"') {
                break_char = '"';
                c++;
            }
            if (*c != '\0') {
                arg_count++;
                ret[arg_count] = ret_str;
            }
        } else {
            *ret_str = *c;
            ret_str++;

            //
            //  If we hit a break char, we count the argument then.
            //  If we hit end of string, count it here; note we're
            //  only counting it if we counted a character before it
            //  (ie., trailing whitespace is not an arg.)
            //

            c++;

            if (*c == '\0') {
                *ret_str = '\0';
            }
        }
    }

    return ret;
}

int __cdecl main(int argc, LPTSTR argv[]);

VOID __cdecl mainCRTStartup()
{
    CHAR ** argv;
    int argc;
    int ret;

    argv = tcmdlinetoargs(GetCommandLine(), &argc);
    ret = main(argc, argv);

    ExitProcess(ret);
}

