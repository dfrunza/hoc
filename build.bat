::        MSVC
:: --------------------
@ECHO off

cl > NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
  ECHO Setting up the MSVC environment
  call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x86 > NUL 2>&1
)
cl > NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
  ECHO MSVC environment is not in working state
  GOTO :early_exit
)

SETLOCAL

SET base_dir=%cd%
SET hoc_file=test
SET hoc_dir=%base_dir%\hoc
SET src_dir=%base_dir%\src

:: /wd4706       - assignment within conditional expression
:: /wd4013       - undefined function (missing forward declaration)
:: /wd4211       - nonstandard extension used (related to wd4013)
:: /wd4306       - 'type cast' : conversion from 'uint8' to 'int32 *' of greater size
:: /wd4459       - declaration hides global declaration
:: /wd4456       - declaration hides previous local declaration
:: /FC           - display full path of source code files passed to cl.exe in diagnostic text
:: /Fo:<path>    - compile to object file
:: /Fe:<path>    - compile to executable
:: /c            - compile without linking
:: /EHa-         - disable exceptions
:: /GR-          - disable RTTI
:: /GS-          - disable buffer security check
:: /W{n}         - warning output level (/W0 disable all warnings)
:: /Wall         - display all warnings

SET C_flags=/Od /W4 /nologo /MTd /Zo /Zi /Gm- /GS- /GR- /EHa- /FC /D_CRT_SECURE_NO_WARNINGS ^
            /wd4201 /wd4127 /wd4100 /wd4706 /wd4211 /wd4306 /wd4459 /wd4456
SET L_flags=/incremental:no /opt:ref /subsystem:console

IF NOT EXIST .\bin (
  ECHO Creating the .\bin directory
  mkdir .\bin
)
PUSHD .\bin

cl %C_flags% %src_dir%\hocc.c /link %L_flags%

IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

:: NOTE: The full path to the .hoc source is needed in order for Vim QuickFix to work properly.

ECHO %hoc_file%.hoc
hocc %hoc_dir%\%hoc_file%.hoc
IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

:: /Cx     - preserve case in publics, externs
:: /Zi     - add symbolic debug info
:: /Fl     - generate listing
:: /c      - assemble without linking
ml /nologo /Cx /Zi /Fl %hoc_file%.asm ^
   /link /nologo /subsystem:console /incremental:no /entry:startup kernel32.lib

:end
POPD

cloc.exe %src_dir%\hocc.h %src_dir%\hocc.c %src_dir%\lib.c %src_dir%\platform.h %src_dir%\translate.c ^
  %src_dir%\lex.c %src_dir%\syntax.c  %src_dir%\sym.c %src_dir%\type.c %src_dir%\ir_gen.c %src_dir%\x86_gen.c

:early_exit

