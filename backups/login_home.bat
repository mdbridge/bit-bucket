::
:: This Windows batch file runs when I login at home
::
:: Ground truth lives in: ~/backups
::


START %HOME%\AutoHotKey\detect.ahk
START %HOME%\AutoHotKey\kill.ahk
:: START %HOME%\AutoHotKey\volume.ahk

START c:\cygwin64\bin\XWin.exe -listen tcp -nohostintitle -ac -multiwindow -clipboard

:: START "starting putty" "C:\Program Files (x86)\PuTTY\putty.exe" -load no-password -l mdl 192.168.1.2

rmdir C:\Users\Mark\Documents\scratch\imports /s /q
mkdir C:\Users\Mark\Documents\scratch\imports
