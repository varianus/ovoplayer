@ECHO OFF
setlocal

:: Set path where lazarus is installed
set LAZARUS_DIR=c:\ex-d\lazarus
set FPC_BIN=C:\Ex-D\lazarus\fpc\2.4.4\bin\i386-Win32
:: 
if "%BASE%" == "" ( SET BASE=%CD%)
   
if not "%OS_TARGET%" == "" ( set DC_ARCH=%DC_ARCH% --os=%OS_TARGET%) ELSE (for /f "usebackq delims=" %%i in (`%FPC_BIN%\fpc -iSO`) do set DC_ARCH=%DC_ARCH% --os=%%i)  
   
if not "%CPU_TARGET%" == "" ( set DC_ARCH=%DC_ARCH% --cpu=%CPU_TARGET% ) ELSE (for /f "usebackq delims=" %%i in (`%FPC_BIN%\fpc -iTP`) do set DC_ARCH=%DC_ARCH% --cpu=%%i)  

if not "%WIDGETSET_TARGET%" == "" (  set DC_ARCH=%DC_ARCH% --ws=%WIDGETSET_TARGET% )   

set NONE=-l
:: clean build files
del /Q /S %BASE\src\lib\*.*
del /Q /S %BASE\src\components\lib\*.*

del /Q ovoplayer.exe
::
copy /Y %BASE%\release.cfg  %BASE%\extrafpc.cfg
%LAZARUS_DIR%\lazbuild -B -r %BASE%\src\ovoplayer.lpi %DC_ARCH%
%FPC_BIN%\strip --strip-all %BASE%\bin\win32\ovoplayer.exe

echo %none% > %BASE%\extrafpc.cfg
ENDLOCAL