@ECHO OFF
setlocal
:: Set path to Innos Setup compiler
set INNOCOMPILER="C:\Program Files (x86)\Inno Setup 5\iscc.exe"

:: Checks
if not exist %INNOCOMPILER% (echo === ERROR ===
						                 echo Missing %INNOCOMPILER%. Please edit "%~nx0" and set correct paths
                             goto :close) 

::Compiling
if "%BASE_SRC%" == "" (SET BASE_SRC=%CD%)
set PACKAGES_DIR=%BASE_SRC%\packages

call %BASE_SRC%\buildwin.bat
IF ERRORLEVEL 1 goto close

call %INNOCOMPILER%  %PACKAGES_DIR%\ovoplayer.iss

:close
endlocal