:: 
:: Copy private Vocola extensions and Vocola version into place from
:: ./setup/{voice,Vocola} (usually PC ~/setup/voice); also copies
:: personal NatLink macros.
::
:: Also attempts to set HOME.
:: 



:: Personal version of Vocola:

.\setup\Vocola\install.bat


:: Private extensions:

copy .\voice\extensions\*           C:\NatLink\NatLink\Vocola\extensions\


:: NatLink macros:

copy .\voice\NatLink\_alias.py      C:\NatLink\NatLink\MacroSystem\
copy .\voice\NatLink\_logging.py    C:\NatLink\NatLink\MacroSystem\


:: Attempt to set HOME:

setx HOME %HOMEDRIVE%%HOMEPATH%\Documents


pause
