@ECHO OFF
setlocal
::
if "%BASE%" == "" (SET BASE=%CD%)

call %BASE%\buildwin.bat

set /P PROG_VER=< %BASE%\src\version.inc
set PROG_VER=%PROG_VER:~1,-1%
set BIN_DIR=%BASE%\bin\win32
set ZIPSRCDIR=%BASE%\packages\zip\win32
set PACKAGES_DIR=%BASE%\packages

rd /s /q %ZIPSRCDIR% > nul
MKDIR %ZIPSRCDIR%
MKDIR %ZIPSRCDIR%\locale
MKDIR %ZIPSRCDIR%\resources		
copy /Y  %BIN_DIR%\ovoplayer.exe %ZIPSRCDIR%
copy /Y  %BASE%\language\*.po %ZIPSRCDIR%\locale
copy /Y  %BASE%\images\logo.png %ZIPSRCDIR%\resources		
copy /Y  %BASE%\images\nocover.png %ZIPSRCDIR%\resources
copy /Y  %BASE%\images\volume-slider.png %ZIPSRCDIR%\resources
copy /Y  %BASE%\images\volume-slider-mask.png %ZIPSRCDIR%\resources		
del /q  %PACKAGES_DIR%\ovoplayer-%PROG_VER%-%FULL_TARGET%.zip 
cd /d %ZIPSRCDIR%
zip -9 -r  %PACKAGES_DIR%\ovoplayer-%PROG_VER%-%FULL_TARGET%.zip .
cd /d %BASE%
rd /S /Q %ZIPSRCDIR% > nul


endlocal