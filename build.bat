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

set prog=test

cl /c %C_flags% ..\lib.c ..\lex.c ..\syntax.c ..\semantic.c ..\typecheck.c /Fo
cl %C_flags% ..\hocc.c lib.obj lex.obj syntax.obj semantic.obj typecheck.obj /link %L_flags%

rem NOTE: The full path to the .hoc source is needed for Vim QuickFix to work properly.
if %errorlevel% neq 0 goto :end
hocc.exe %cd%\..\%prog%.hoc > debug.txt

rem if %errorlevel% neq 0 goto :hocc_error
rem ..\ctime.exe cl %C_flags% ..\vm.c %prog%.res /link %L_flags% /Fe:%prog%.exe 

goto :end

:hocc_error
echo hocc.exe error
goto :end

:end
popd

cloc.exe hasm.h lex.h lib.h runtime_obj.h syntax.h codegen.c hasm.c ^
hocc.c lex.c lib.c runtime_obj.c semantic.c semantic.h syntax.c typecheck.c vm.c


