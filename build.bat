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
  ECHO MSVC environment is not functional
  GOTO :early_exit
)

SETLOCAL

SET base_dir=%cd%
SET hoc_dir=%base_dir%\hoc
SET src_dir=%base_dir%\src

:: /wd4706       - assignment within conditional expression
:: /wd4013       - undefined function (missing forward declaration)
:: /wd4211       - nonstandard extension used (related to wd4013)
:: /wd4306       - 'type cast' : conversion from 'uint8' to 'int32 *' of greater size
:: /wd4459       - declaration hides global declaration
:: /wd4456       - declaration hides previous local declaration
:: /wd4533       - initialization of '..' is skipped by 'goto ..'
:: /wd4458       - declaration of '..' hides class member
:: /FC           - display full path of source code files passed to cl.exe in diagnostic text
:: /Fo:<path>    - compile to object file
:: /Fe:<path>    - compile to executable
:: /c            - compile without linking
:: /EHa-         - disable exceptions
:: /GR-          - disable RTTI
:: /GS-          - disable buffer security check
:: /W{n}         - warning output level (/W0 disable all warnings)
:: /Wall         - display all warnings

SET C_flags=/Od /MTd /W4 /Zo /Zi /Gm- /GS- /GR- /EHa- /FC /nologo /D_CRT_SECURE_NO_WARNINGS ^
            /wd4201 /wd4127 /wd4100 /wd4706 /wd4211 /wd4306 /wd4459 /wd4456 /wd4533 /wd4458
SET L_flags=/incremental:no /opt:ref /subsystem:console

IF NOT EXIST .\bin (
  ECHO Creating the .\bin directory
  mkdir .\bin
)
PUSHD .\bin

::..\ctime -begin hocc.ctm
cl %C_flags% %src_dir%\hocc.c /link %L_flags%
::..\ctime -end hocc.ctm

IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

hocc %hoc_dir%\test.hoc
IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

hocc %hoc_dir%\test-0_2.hoc
IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

hocc %hoc_dir%\sloc.hoc
IF %ERRORLEVEL% NEQ 0 (
  GOTO :end
)

:end
POPD

:early_exit

