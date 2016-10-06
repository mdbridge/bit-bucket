::
:: This Windows batch file runs when I login at home
::
:: Ground truth lives in: ~/backups
::


START %HOME%\AutoHotKey\detect.ahk
START %HOME%\AutoHotKey\kill.ahk
:: START %HOME%\AutoHotKey\volume.ahk

START c:\cygwin64\bin\XWin.exe -listen tcp -nohostintitle -ac -multiwindow -clipboard

::START "connecting foil" "C:\Program Files (x86)\ExpanDrive\ExpanDrive.exe" connect foil

:: START "starting putty" "C:\Program Files (x86)\PuTTY\putty.exe" -load no-password -l mdl 192.168.1.2
