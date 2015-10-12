@ECHO OFF
setlocal
:: Set path where FPC binaries are installed
set ZIPCMD="C:\Ex-D\lazarus\fpc\3.1.1\bin\i386-Win32\zip.exe"

:: Checks

if not exist %ZIPCMD% (echo === ERROR ===
		       Echo Missing %ZIPCMD% . Please edit "%~nx0" and set correct paths
                       goto :close) 
										   
:: 
if "%BASE_SRC%" == "" (SET BASE_SRC=%CD%)
set /P PROG_VER=< %BASE_SRC%\src\version.inc
set PROG_VER=%PROG_VER:~1,-1%
set BIN_DIR=%BASE_SRC%\bin\win32
set ZIPSRCDIR=%BASE_SRC%\packages\zip\win32
set PACKAGES_DIR=%BASE_SRC%\packages

call %BASE_SRC%\buildwin.bat
if ERRORLEVEL 1 goto :close

rd /s /q %ZIPSRCDIR% > nul
MKDIR %ZIPSRCDIR%
MKDIR %ZIPSRCDIR%\locale
MKDIR %ZIPSRCDIR%\resources		
copy /Y  %BIN_DIR%\ovoplayer.exe %ZIPSRCDIR%
copy /Y  %BIN_DIR%\ovoplayerctrl.exe %ZIPSRCDIR%
copy /Y  %BIN_DIR%\sqlite3.dll %ZIPSRCDIR%
copy /Y  %BASE_SRC%\language\*.po %ZIPSRCDIR%\locale
copy /Y  %BASE_SRC%\images\logo.png %ZIPSRCDIR%\resources		
copy /Y  %BASE_SRC%\images\nocover.png %ZIPSRCDIR%\resources
copy /Y  %BASE_SRC%\images\volume-slider.png %ZIPSRCDIR%\resources
copy /Y  %BASE_SRC%\images\volume-slider-mask.png %ZIPSRCDIR%\resources		
del /q  %PACKAGES_DIR%\ovoplayer-%PROG_VER%.zip 
cd /d %ZIPSRCDIR%
%ZIPCMD% -9 -r  %PACKAGES_DIR%\ovoplayer-%PROG_VER%.zip .
cd /d %BASE_SRC%
rd /S /Q %ZIPSRCDIR% > nul

:close
endlocal