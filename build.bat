@echo off

rem /wd4706       - assignment within conditional expression
rem /wd4013       - undefined function (missing forward declaration)
rem /wd4211       - nonstandard extension used (related to wd4013)
rem /wd4306       - 'type cast' : conversion from 'uint8' to 'int32 *' of greater size
rem /Fo:<path>    - compile to object file
rem /Fe:<path>    - compile to executable
rem /c            - compile without linking
rem /EHa-         - disable exceptions
rem /GR-          - disable RTTI
rem /W{n}         - warning output level (/W0 disable all warnings)
rem /Wall         - display all warnings

set C_flags=/Od /W4 /nologo /MTd /Zo /Zi /Gm- /GR- /EHa- /FC /D_CRT_SECURE_NO_WARNINGS ^
                  /wd4201 /wd4127 /wd4100 /wd4706 /wd4211 /wd4306
set L_flags=/incremental:no /opt:ref /subsystem:console

if not exist .\bin mkdir .\bin
pushd .\bin

set hoc_file=test
set hoc_dir=%cd%\..\hoc
set src_dir=%cd%\..\src

..\ctime.exe ^
cl %C_flags% %src_dir%\hocc.c /link %L_flags%
if %errorlevel% neq 0 goto :end

rem ..\ctime.exe ^
rem cl %C_flags% ..\vm.c /link %L_flags%
rem if %errorlevel% neq 0 goto :end

rem NOTE: The full path to the .hoc source is needed in order for Vim QuickFix to work properly.
echo  Compiling: %hoc_file%.hoc
hocc %hoc_dir%\%hoc_file%.hoc > debug.txt
if %errorlevel% neq 0 goto :hocc_error

rem /Cx     - preserve case in publics, externs
rem /Zi     - add symbolic debug info
rem /Fl     - generate listing
ml /nologo /Cx /Zi /Fl %hoc_file%.asm ^
/link /nologo /subsystem:console /incremental:no /entry:start kernel32.lib

rem cl %C_flags% ..\fp3.c /link %L_flags%

goto :end

:hocc_error
echo %cd%(0) : hocc.exe error
goto :end

:end
popd

cloc.exe %src_dir%\hocc.h %src_dir%\hocc.c %src_dir%\lib.c %src_dir%\platform.h %src_dir%\platform.c %src_dir%\translate.c ^
  %src_dir%\lex.c %src_dir%\syntax.c %src_dir%\semantic.c %src_dir%\type.c %src_dir%\runtime.c %src_dir%\x86.c


