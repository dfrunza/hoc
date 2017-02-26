@echo off

if not exist .\bin mkdir .\bin
pushd .\bin

set CompilerFlags=-Od -W4 -wd4201 -wd4127 -wd4100 -MTd -Zo -Zi -Gm- -GR- -EHa- -FC -D_CRT_SECURE_NO_WARNINGS -nologo 
set LinkerFlags=-incremental:no -opt:ref -subsystem:console user32.lib

set Program=test
cl %CompilerFlags% ..\hocc.cpp /link %LinkerFlags%
hocc.exe %cd%\..\%Program%.hoc

if %errorlevel% neq 0 goto :build_failed
cl /Fe:%Program%.exe %CompilerFlags% ..\vm.cpp %Program%.res /link %LinkerFlags% 

echo Build successful
exit

:build_failed
echo Build failed
popd
