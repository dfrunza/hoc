@ECHO off
REM -----    GCC   ------

SET C_flags=-g -ggdb
SET L_flags=

IF NOT EXIST .\bin mkdir .\bin
PUSHD .\bin

SET hoc_file=test
SET hoc_dir=%cd%\..\hoc
SET src_dir=%cd%\..\src

REM If you use -nostdlib, you get an unresolved reference to __main, since it's defined in the standard GCC library.
REM Include -lgcc at the end of your compiler command line to resolve this reference.
REM Calling __main is necessary, even when compiling C code, to allow linking C and C++ object code together.
..\ctime.exe ^
gcc -std=c99 -nostdlib -static %C_flags% %src_dir%\hocc.c ^
-lminicrt -lkernel32 -lgcc -Wl,-e_mainCRTStartup -o hocc.exe
IF %errorlevel% NEQ 0 GOTO :end

REM NOTE: The full path to the .hoc source is needed in order for Vim QuickFix to work properly.
ECHO  Compiling: %hoc_file%.hoc
hocc %hoc_dir%\%hoc_file%.hoc > debug.txt
IF %errorlevel% NEQ 0 GOTO :hocc_error

REM /Cx     - preserve case in publics, externs
REM /Zi     - add symbolic debug info
REM /Fl     - generate listing
REM ml /nologo /Cx /Zi /Fl %hoc_file%.asm ^
REM /link /nologo /subsystem:console /incremental:no /entry:startup kernel32.lib

REM cl %C_flags% ..\fp3.c /link %L_flags%

GOTO :end

:hocc_error
ECHO hocc error
GOTO :end

:end
POPD

cloc.exe %src_dir%\hocc.h %src_dir%\hocc.c %src_dir%\lib.c %src_dir%\platform.h %src_dir%\translate.c ^
  %src_dir%\lex.c %src_dir%\syntax.c


