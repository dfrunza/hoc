@ECHO off

gcc.exe -v > NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
  ECHO gcc.exe not available
  GOTO :early_exit
)

ml.exe > NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
  ECHO ml.exe not available
  GOTO :early_exit
)

SETLOCAL

SET base_dir=%cd%
SET hoc_dir=%base_dir%\hoc
SET src_dir=%base_dir%\src

::      CRT removal note
:: ---------------------------
:: If you use -nostdlib, you get an unresolved reference to __main, since it's defined in the standard GCC library.
:: Include -lgcc at the end of your compiler command line to resolve this reference.
:: Calling __main is necessary, even when compiling C code, to allow linking C and C++ object code together.
:: g++ -nostdlib -static %C_flags% %src_dir%\hocc.cpp -lkernel32 -lgcc -Wl,-e_mainCRTStartup -o hocc.exe
:: ---------------------------

:: -Wno-write-strings      - ISO C++ forbids converting a string constant to 'char*'

SET C_flags=-g -ggdb -std=c99 -Winline -Wno-write-strings
SET L_flags=

IF NOT EXIST .\bin (
  ECHO Creating the .\bin directory
  mkdir .\bin
)
PUSHD .\bin

::..\ctime -begin hocc.ctm
gcc %C_flags% %src_dir%\hocc.c -lkernel32 -o hocc.exe
::..\ctime -end hocc.ctm

IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

hocc %hoc_dir%\test.hoc
IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

hocc %hoc_dir%\test-0_2.hoc
IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

hocc %hoc_dir%\sloc.hoc
IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

:end
POPD

:early_exit

