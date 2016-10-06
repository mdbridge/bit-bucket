@ECHO OFF

@ Indirection script for "new Cygwin xterm" voice command

C:\cygwin64\bin\xterm -display localhost:0.0 -fa BitstreamVeraSansMono -fs 10 -title 'Cygwin xterm' -n 'Cygwin xterm' -e 'c:/cygwin64/bin/tcsh -l'
