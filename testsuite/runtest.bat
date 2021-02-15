@echo off
REM Test suite Configuration parameters (FPCDIR and FPCBIN)
REM These may be modified if needed to suite local requirements

set LAZARUS=C:\lazarus
FOR %%V in (3.2.0 3.0.4 3.0.2 3.0.0) do (
  if EXIST C:\lazarus\fpc\%%V\bin\i386-win32\fpc.exe (
    set FPCDIR=C:\lazarus\fpc\%%V
    set FPCBIN=C:\lazarus\fpc\%%V\bin\i386-win32
    Goto COMPILE
  )
  if EXIST C:\lazarus\fpc\%%V\bin\x86_64-win32\fpc.exe (
    set FPCDIR=C:\lazarus\fpc\%%V
    set FPCBIN=C:\lazarus\fpc\%%V\bin\x86_64-win32
    Goto COMPILE
  )
  if EXIST C:\lazarus\fpc\%%V\bin\x86_64-win64\fpc.exe (
    set FPCDIR=C:\lazarus\fpc\%%V
    set FPCBIN=C:\lazarus\fpc\%%V\bin\x86_64-win64
    Goto COMPILE
  )
)

if not EXIST %FPCBIN%\fpc.exe (
  echo "Unable to find fpc.exe"
  goto :EOF
)

:COMPILE
set TESTOUTDIR=%TEMP%\ibx-testsuite
set USERNAME=SYSDBA
set PASSWORD=masterkey
set EMPLOYEEDB=employee
set NEWDBNAME=%TESTOUTDIR%\testsuite1.fdb
set NEWDBNAME2=%TESTOUTDIR%\testsuite2.fdb
set BAKFILE=%TESTOUTDIR%\testsuite.gbk
set DIFF=%FPCBIN%\diff.exe

rd /s /q testunits
mkdir %TESTOUTDIR%

IF EXIST "..\fbintf" (
  set FBINTF=../fbintf
  goto COMPILE2
 )
if EXIST "..\..\fbintf" (
  set FBINTF=../../fbintf
  goto COMPILE2
 )
echo Error: unable to locate Pascal Firebird Interface API"
goto :EOF

:COMPILE2
set INCDIR=%FBINTF%/client/3.0/firebird %FBINTF%/client/include
set UNITDIR=%FBINTF% %FBINTF%/client %FBINTF%/client/3.0/firebird %FBINTF%/client/2.5 %FBINTF%/client/3.0  %LAZARUS%/components/lazutils

echo UNITDIR= %UNITDIR%

%FPCBIN%\fpcmake
%FPCBIN%\make clean
%FPCBIN%\make INCDIR="%INCDIR%" UNITDIR="%UNITDIR%"
echo( 
echo Starting Testsuite
echo( 
IF EXIST "testsuite.exe" (
testsuite.exe -u %USERNAME% -p %PASSWORD% -e %EMPLOYEEDB% -n %NEWDBNAME% -s %NEWDBNAME2% -b %BAKFILE% -o testout.log %1

if not EXIST "%DIFF%" (
  echo Unable to compare results - diff not found - %DIFF%
  goto :EOF
  )

echo Comparing results with reference log
echo(
findstr /C:"ODS Major Version = 11" testout.log
IF ERRORLEVEL 1 (
  findstr /C:"ODS Major Version = 12" testout.log
  IF ERRORLEVEL 1 (
    %DIFF% FB4reference.log testout.log >diff.log
  ) ELSE (
    %DIFF% FB3reference.log testout.log >diff.log
  )
) ELSE (
  %DIFF% FB2reference.log testout.log >diff.log
)
type diff.log
rd /s /q testunits
del testsuite.exe
)
