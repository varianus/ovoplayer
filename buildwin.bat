@ECHO OFF
setlocal

:: Set path where Lazarus is installed
set LAZARUS_DIR=c:\ex-d\lazarus
:: Set path where FPC binaries are installed
set FPC_BIN=C:\Ex-D\lazarus\fpc\2.6.4\bin\i386-Win32
::  Primary config path - needed if you have installed lazarus with fpcup or have multiple installation
set CONFIG_PATH=

:: Checks
if not exist "%LAZARUS_DIR%\lazbuild.exe" (echo === ERROR ===
  	                                       echo Missing "%LAZARUS_DIR%\lazbuild.exe". Please edit "%~nx0" and set correct paths
                                           goto :close_bad) 

if not exist "%FPC_BIN%\strip.exe" ( echo === ERROR ===
                                     echo Missing "%FPC_BIN%\strip.exe". Please edit "%~nx0" and set correct paths
                                     goto :close_bad) 

:: Building process
:build
if "%BASE_SRC%" == "" ( SET BASE_SRC=%CD%)
if not "%OS_TARGET%" == "" ( set DC_ARCH=%DC_ARCH% --os=%OS_TARGET%) ELSE (for /f "usebackq delims=" %%i in (`%FPC_BIN%\fpc -iSO`) do set DC_ARCH=%DC_ARCH% --os=%%i)  
if not "%CPU_TARGET%" == "" ( set DC_ARCH=%DC_ARCH% --cpu=%CPU_TARGET% ) ELSE (for /f "usebackq delims=" %%i in (`%FPC_BIN%\fpc -iTP`) do set DC_ARCH=%DC_ARCH% --cpu=%%i)  
if not "%WIDGETSET_TARGET%" == "" (  set DC_ARCH=%DC_ARCH% --ws=%WIDGETSET_TARGET% )   
if not "%CONFIG_PATH%" == "" ( set PCP=--pcp="%CONFIG_PATH%") else (set PCP=)

set NONE=-l
:: clean build files
del /Q /S %BASE_SRC%\src\lib\*.*
del /Q /S %BASE_SRC%\src\components\lib\*.*
del /Q /S %BASE_SRC%\tools\ovoplayerctrl\lib\*.*

del /Q ovoplayer.exe
del /Q ovoplayerctrl.exe
::
copy /Y %BASE_SRC%\release.cfg  %BASE_SRC%\extrafpc.cfg
%LAZARUS_DIR%\lazbuild -B -r %PCP% --build-mode=Release --lazarusdir=%LAZARUS_DIR% %DC_ARCH% %BASE_SRC%\src\ovoplayer.lpi 
if ERRORLEVEL 1 goto :close_bad
%LAZARUS_DIR%\lazbuild -B -r %PCP% --lazarusdir=%LAZARUS_DIR% %DC_ARCH% %BASE_SRC%\tools\ovoplayerctrl\ovoplayerctrl.lpi
if ERRORLEVEL 1 goto :close_bad
%FPC_BIN%\strip --strip-all %BASE_SRC%\bin\win32\ovoplayer.exe
if ERRORLEVEL 1 goto :close_bad
%FPC_BIN%\strip --strip-all %BASE_SRC%\bin\win32\ovoplayerctrl.exe
if ERRORLEVEL 1 goto :close_bad

goto :close
:close_bad
exit /b 1
:close
echo %none% > %BASE_SRC%\extrafpc.cfg
ENDLOCAL