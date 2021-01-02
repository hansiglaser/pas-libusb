REM delete all compiler created interim files :
REM -------------------------------------------
REM /Q = quiet mode, do not ask if ok to delete on global wildcard
REM /S = delete specified files from all subdirectories. 


rem Delete the EXE files :
rem ------------------------------
del "%~dp0\exeOutput\GUITest1Library.exe"


REM delete Lazarus temporary output files :
REM ---------------------------------------
del /Q/S "%~dp0\*.a"
del /Q/S "%~dp0\*.o"
del /Q/S "%~dp0\*.or"
del /Q/S "%~dp0\*.ppu"
del /Q/S "%~dp0\*.compiled"


REM delete Lazarus backup files :
REM ----------------------------------
del /Q/S "%~dp0\*.bak"
del /Q/S "%~dp0\backup\*.*"
del /Q/S "%~dp0\units\backup\*.*"
del /Q/S "%~dp0\units\Const\backup\*.*"
del /Q/S "%~dp0\units\Objects\backup\*.*"
del /Q/S "%~dp0\output\*.*"
del /Q/S "%~dp0\units\SoundEngine\Audio\backup\*.*"
del /Q/S "%~dp0\units\SoundEngine\DSP\backup\*.*"
del /Q/S "%~dp0\units\Types\backup\*.*"


REM delete some temporary output folders :
REM ---------------------------------------
rmdir "%~dp0\backup\backup"
rmdir "%~dp0\backup"
rmdir "%~dp0\pas-libusb\backup"


pause