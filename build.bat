@echo off

rem -wd4706       - assignment within conditional expression
rem -wd4013       - undefined function (missing forward declaration)
rem -wd4211       - nonstandard extension used (related to wd4013)
rem -wd4306       - 'type cast' : conversion from 'uint8' to 'int32 *' of greater size
rem /Fo:<path>    - compile to object file
rem /Fe:<path>    - compile to executable
rem /c            - compile without linking
rem /EHa-         - disable exceptions
rem /GR-          - disable RTTI
rem /W{n}         - warning output level (-W0 disable all warnings)
rem /Wall         - display all warnings

if not exist .\bin mkdir .\bin
pushd .\bin

set C_flags=/Od /W4 /nologo /MTd /Zo /Zi /Gm- /GR- /EHa- /FC /D_CRT_SECURE_NO_WARNINGS ^
                  /wd4201 /wd4127 /wd4100 /wd4706 /wd4211 /wd4306
set L_flags=-incremental:no -opt:ref -subsystem:console

..\ctime.exe ^
cl %C_flags% ..\hocc.cpp /link %L_flags%
if %errorlevel% neq 0 goto :end

..\ctime.exe ^
cl %C_flags% ..\vm.cpp /link %L_flags%
if %errorlevel% neq 0 goto :end

rem NOTE: The full path to the .hoc source is needed in order for Vim QuickFix to work properly.
set hoc_src=%cd%\..\hoc
echo Compiling HoC Code...
hocc %hoc_src%\test.hoc > debug.txt
if %errorlevel% neq 0 goto :hocc_error

rem cl %C_flags% ..\fp3.cpp /link %L_flags%

goto :end

:hocc_error
echo hocc.exe error
goto :end

:end
popd

cloc.exe hocc.h codegen.cpp hasm.cpp hocc.cpp lex.cpp lib.cpp runtime.cpp semantic.cpp syntax.cpp type.cpp vm.cpp


