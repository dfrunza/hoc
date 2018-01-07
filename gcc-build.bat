@ECHO off
REM        GCC
REM --------------------

SET base_dir=%cd%
IF NOT EXIST .\bin mkdir .\bin
PUSHD .\bin

SET C_flags=-g -ggdb
SET L_flags=

SET hoc_file=test
SET hoc_dir=%base_dir%\hoc
SET src_dir=%base_dir%\src

REM         Without CRT
REM ---------------------------
REM If you use -nostdlib, you get an unresolved reference to __main, since it's defined in the standard GCC library.
REM Include -lgcc at the end of your compiler command line to resolve this reference.
REM Calling __main is necessary, even when compiling C code, to allow linking C and C++ object code together.
REM gcc -std=c99 -nostdlib -static %C_flags% %src_dir%\hocc.c -lkernel32 -lgcc -Wl,-e_mainCRTStartup -o hocc.exe
REM ---------------------------

REM ..\ctime.exe ^
gcc -std=c99 %C_flags% %src_dir%\hocc.c -lkernel32 -o hocc.exe
IF %errorlevel% NEQ 0 GOTO :end

REM NOTE: The full path to the .hoc source is needed in order for Vim QuickFix to work properly.
ECHO %hoc_file%.hoc
hocc %hoc_dir%\%hoc_file%.hoc > debug.txt
IF %errorlevel% NEQ 0 GOTO :hocc_error

REM /Cx     - preserve case in publics, externs
REM /Zi     - add symbolic debug info
REM /Fl     - generate listing
ml /nologo /Cx /Zi /Fl %hoc_file%.asm ^
   /link /nologo /subsystem:console /incremental:no /entry:startup kernel32.lib

:end
POPD

cloc.exe %src_dir%\hocc.h %src_dir%\hocc.c %src_dir%\lib.c %src_dir%\platform.h %src_dir%\translate.c ^
  %src_dir%\lex.c %src_dir%\syntax.c


