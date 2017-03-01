@echo off

if not exist .\bin mkdir .\bin
pushd .\bin

rem wd4706 : assignment within conditional expression
set CompilerFlags=-Od -W4 -nologo -MTd -Zo -Zi -Gm- -GR- -EHa- -FC -D_CRT_SECURE_NO_WARNINGS ^
                  -wd4201 -wd4127 -wd4100 -wd4706
set LinkerFlags=-incremental:no -opt:ref -subsystem:console

set Program=test

cl %CompilerFlags% ..\hocc.cpp /link %LinkerFlags%

if %errorlevel% neq 0 goto :build_failed
hocc.exe %cd%\..\%Program%.hoc

if %errorlevel% neq 0 goto :hocc_exe_error
cl /Fe:%Program%.exe %CompilerFlags% ..\vm.cpp %Program%.res /link %LinkerFlags% 

echo Build successful
exit

:build_failed
echo Build failed

:hocc_exe_error
echo hocc.exe exited with error
popd
