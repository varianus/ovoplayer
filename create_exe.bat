@ECHO OFF
setlocal
::
set INNOCOMPILER="C:\Program Files (x86)\Inno Setup 5\iscc.exe"
::
if "%BASE%" == "" (SET BASE=%CD%)
set PACKAGES_DIR=%BASE%\packages

call %BASE%\buildwin.bat

call %INNOCOMPILER%  %PACKAGES_DIR%\ovoplayer.iss

endlocal