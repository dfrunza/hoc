@echo off

if not exist .\bin mkdir .\bin
pushd .\bin

set CompilerFlags=-Od -W4 -wd4201 -wd4127 -wd4100 -MTd -Zo -Zi -Gm- -GR- -EHa- -FC -D_CRT_SECURE_NO_WARNINGS -nologo 
set LinkerFlags=-incremental:no -opt:ref -subsystem:console user32.lib

set Program=test
cl %CompilerFlags% ..\hocc.cpp /link %LinkerFlags%
hocc.exe %cd%\..\%Program%.hoc
cl /Fe:%Program%.exe %CompilerFlags% ..\vm.cpp %Program%.res /link %LinkerFlags% 

popd
echo Build finished
