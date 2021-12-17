@echo off

if exist "C:\Program Files\LispWorks\lispworks-8-0-0-x86-win32.exe" (
   set LW_PATH="C:\Program Files\LispWorks\lispworks-8-0-0-x86-win32.exe" 
) else if exist "C:\Program Files (x86)\LispWorks\lispworks-8-0-0-x86-win32.exe" (
   set LW_PATH="C:\Program Files (x86)\LispWorks\lispworks-8-0-0-x86-win32.exe" 
) else if exist "C:\Program Files (x86)\LispWorks\lispworks-7-1-0-x86-win32.exe" (
   set LW_PATH="C:\Program Files (x86)\LispWorks\lispworks-7-1-0-x86-win32.exe"
) else if exist "C:\Program Files\LispWorks\lispworks-7-1-0-x86-win32.exe" (
   set LW_PATH="C:\Program Files\LispWorks\lispworks-7-1-0-x86-win32.exe"
) else (
   echo "Unable to find LispWorks executable!"
)
%LW_PATH% -build src/delivery-script.lisp
