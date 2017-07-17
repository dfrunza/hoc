@echo off

rem Memos
rem -wd4706       - assignment within conditional expression
rem -wd4013       - undefined function (missing forward declaration)
rem -wd4211       - nonstandard extension used (related to wd4013)
rem /Fo:<path>    - compile to object file
rem /Fe:<path>    - compile to executable
rem /c            - compile without linking

if not exist .\bin mkdir .\bin
pushd .\bin

set C_flags=-Od -W4 -nologo -MTd -Zo -Zi -Gm- -GR- -EHa- -FC -D_CRT_SECURE_NO_WARNINGS ^
                  -wd4201 -wd4127 -wd4100 -wd4706 -wd4211
set L_flags=-incremental:no -opt:ref -subsystem:console

set PROG=test

..\ctime.exe ^
cl %C_flags% ..\hocc.cc ..\lib.cc ..\lex.cc ..\syntax.cc ..\semantic.cc ..\typecheck.cc ^
  ..\runtime.cc ..\codegen.cc ..\hasm.cc /link %L_flags%

rem NOTE: The full path to the .hoc source is needed for Vim QuickFix to work properly.
if %errorlevel% neq 0 goto :end
hocc.exe %cd%\..\%PROG%.hoc > out_debug.txt

if %errorlevel% neq 0 goto :hocc_error
..\ctime.exe ^
cl %C_flags% /Fe:%PROG%.exe ..\vm.cc ..\lib.cc %PROG%.res /link %L_flags%

goto :end

:hocc_error
echo hocc.exe error
goto :end

:end
popd

cloc.exe hocc.h codegen.cc hasm.cc hocc.cc lex.cc lib.cc runtime.cc semantic.cc syntax.cc typecheck.cc vm.cc


