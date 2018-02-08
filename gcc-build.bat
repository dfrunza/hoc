::        GCC
:: --------------------
@ECHO off

gcc -v > NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
  ECHO Setting up the MINGW environment
  SET "PATH=%PATH%;d:\mingw\bin\"
)
gcc -v > NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
  ECHO MINGW environment is not functional
  GOTO :early_exit
)

ml > NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
  ECHO Setting up the MSVC environment
  call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x86 > NUL 2>&1
)
ml > NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
  ECHO MSVC environment is not functional
  GOTO :early_exit
)

SETLOCAL

SET base_dir=%cd%
SET hoc_file=test
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

SET C_flags=-g -ggdb -Winline -Wno-write-strings
SET L_flags=

IF NOT EXIST .\bin (
  ECHO Creating the .\bin directory
  mkdir .\bin
)
PUSHD .\bin

g++ %C_flags% %src_dir%\hocc.cpp -lkernel32 -o hocc.exe

IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

:: NOTE: The full path to the .hoc source is needed in order for Vim QuickFix to work properly.
ECHO %hoc_file%.hoc
hocc %hoc_dir%\%hoc_file%.hoc > debug.txt

IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

:: /Cx     - preserve case in publics, externs
:: /Zi     - add symbolic debug info
:: /Fl     - generate listing
ml /nologo /Cx /Zi /Fl %hoc_file%.asm ^
   /link /nologo /subsystem:console /incremental:no /entry:startup kernel32.lib

:end
POPD

:early_exit

