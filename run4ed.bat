@echo off
if "%VCINSTALLDIR%" neq "" goto :skip_env
rem call msdevx64.bat
  call msdevx86.bat
:skip_env

start "" "c:\tools\4coder\4ed.exe" -f 15
