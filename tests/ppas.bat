@echo off
SET THEFILE=TestMachine.exe
echo Linking %THEFILE%
C:\lazarus\fpc\3.0.0\bin\i386-win32\ld.exe -b pei-i386 -m i386pe  --gc-sections  -s  --entry=_mainCRTStartup    -o TestMachine.exe link.res
if errorlevel 1 goto linkend
C:\lazarus\fpc\3.0.0\bin\i386-win32\postw32.exe --subsystem console --input TestMachine.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
