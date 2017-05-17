@echo off

if not exist .\bin mkdir .\bin
pushd .\bin

rem wd4706 : assignment within conditional expression
set CompilerFlags=-Od -W4 -nologo -MTd -Zo -Zi -Gm- -GR- -EHa- -FC -D_CRT_SECURE_NO_WARNINGS ^
                  -wd4201 -wd4127 -wd4100 -wd4706
set LinkerFlags=-incremental:no -opt:ref -subsystem:console

set Program=test

..\ctime.exe cl %CompilerFlags% ..\hocc.c /link %LinkerFlags%

if %errorlevel% neq 0 goto :build_failed
..\ctime.exe hocc.exe %cd%\..\%Program%.hoc

if %errorlevel% neq 0 goto :hocc_exe_error
..\ctime.exe cl /Fe:%Program%.exe %CompilerFlags% ..\vm.c %Program%.res /link %LinkerFlags% 

rem echo Build successful
goto :end

:build_failed
echo Build failed
goto :end

:hocc_exe_error
echo hocc.exe exited with error code
goto :end

:end
popd

cloc.exe lib.c hasm.h hasm.c lex.h lex.c syntax.h syntax.c typecheck.h typecheck.c ^
runtime_obj.h runtime_obj.c codegen.c hocc.c translate.c vm.c 
