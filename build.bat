@ECHO off
REM        MSVC
REM --------------------

REM /wd4706       - assignment within conditional expression
REM /wd4013       - undefined function (missing forward declaration)
REM /wd4211       - nonstandard extension used (related to wd4013)
REM /wd4306       - 'type cast' : conversion from 'uint8' to 'int32 *' of greater size
REM /wd4459       - declaration X hides global declaration
REM /Fo:<path>    - compile to object file
REM /Fe:<path>    - compile to executable
REM /c            - compile without linking
REM /EHa-         - disable exceptions
REM /GR-          - disable RTTI
REM /GS-          - disable buffer security check
REM /W{n}         - warning output level (/W0 disable all warnings)
REM /Wall         - display all warnings

SET C_flags=/Od /W4 /nologo /MTd /Zo /Zi /Gm- /GS- /GR- /EHa- /FC /D_CRT_SECURE_NO_WARNINGS ^
                  /wd4201 /wd4127 /wd4100 /wd4706 /wd4211 /wd4306 /wd4459
SET L_flags=/incremental:no /opt:ref /subsystem:console

IF NOT EXIST .\bin mkdir .\bin
PUSHD .\bin

SET hoc_file=test
SET hoc_dir=%cd%\..\hoc
SET src_dir=%cd%\..\src

..\ctime.exe ^
cl %C_flags% %src_dir%\hocc.c /link %L_flags%
IF %errorlevel% NEQ 0 GOTO :end

REM NOTE: The full path to the .hoc source is needed in order for Vim QuickFix to work properly.
ECHO  Compiling: %hoc_file%.hoc
hocc %hoc_dir%\%hoc_file%.hoc > debug.txt
IF %errorlevel% NEQ 0 GOTO :hocc_error

REM /Cx     - preserve case in publics, externs
REM /Zi     - add symbolic debug info
REM /Fl     - generate listing
REM ml /nologo /Cx /Zi /Fl %hoc_file%.asm ^
REM /link /nologo /subsystem:console /incremental:no /entry:startup kernel32.lib

REM cl %C_flags% ..\fp3.c /link %L_flags%

GOTO :end

:hocc_error
ECHO %cd%:0: hocc error
GOTO :end

:end
POPD

cloc.exe %src_dir%\hocc.h %src_dir%\hocc.c %src_dir%\lib.c %src_dir%\platform.h %src_dir%\translate.c ^
  %src_dir%\lex.c %src_dir%\syntax.c 


