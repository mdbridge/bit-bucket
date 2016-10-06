::
:: Complete setting up PC Emacs
::
::     This needs to be run as administrator
::

assoc .el=elisp_file
      
assoc .vch=Vocola_file
assoc .vcl=Vocola_file


:: Below is only appropriate for 64-bit operating systems:
ftype elisp_file=C:\Program Files (x86)\Emacs\bin\runemacs "%%1"
ftype txtfile=C:\Program Files (x86)\Emacs\bin\runemacs "%%1"
ftype Vocola_file=C:\Program Files (x86)\Emacs\bin\runemacs "%%1"


:: Create send to shortcut:
copy /B /Y %HOME%\setup\emacs.lnk %APPDATA%\Microsoft\Windows\SendTo

:: Desktop shortcut [will move to Icons folder later]
copy /B /Y %HOME%\setup\emacs.lnk %HOME%\..\Desktop
      
pause
