@echo off

rem Memos
rem -wd4706       - assignment within conditional expression
rem /Fo:<path>    - compile to object file
rem /Fe:<path>    - compile to executable
rem /c            - compile without linking

if not exist .\bin mkdir .\bin
pushd .\bin

set C_flags=-Od -W4 -nologo -MTd -Zo -Zi -Gm- -GR- -EHa- -FC -D_CRT_SECURE_NO_WARNINGS ^
                  -wd4201 -wd4127 -wd4100 -wd4706
set L_flags=-incremental:no -opt:ref -subsystem:console

set prog=test

..\ctime cl /c %C_flags% ..\lib.c ..\lex.c ..\syntax.c /Fo
..\ctime cl %C_flags% ..\hocc.c lib.obj lex.obj syntax.obj /link %L_flags%

if %errorlevel% neq 0 goto :build_failed
..\ctime.exe hocc.exe ..\%prog%.hoc > debug.txt

rem if %errorlevel% neq 0 goto :hocc_exe_error
rem ..\ctime.exe cl %C_flags% ..\vm.c %prog%.res /link %L_flags% /Fe:%prog%.exe 

rem echo build successful
goto :end

:build_failed
echo build failed
goto :end

:hocc_exe_error
echo hocc.exe exited with error code
goto :end

:end
popd

cloc.exe lib.c hasm.h hasm.c lex.h lex.c syntax.h syntax.c typecheck.h typecheck.c ^
runtime_obj.h runtime_obj.c codegen.c hocc.c vm.c semantic.c

