default: stage upload

upload: 
	ftp -n -i vocola.net < FTP-commands.txt


ALL= \
    vocola_ext_clipboard.py \
    vocola_ext_date.py \
    vocola_ext_environment.py \
    vocola_ext_Excel.py \
    vocola_ext_keys.py \
    SendInput.py \
    ExtendedSendDragonKeys.py \
    vocola_ext_subprocess.py \
    vocola_ext_variables.py \
    vocola_ext_vocola.py

stage: staging
	cp unofficial.css staging/
	#
	rm -f staging/*.zip
	zip staging/clipboard.zip  vocola_ext_clipboard.py  vocola_ext_variables.py
	zip staging/date.zip	   vocola_ext_date.py
	zip staging/env.zip	   vocola_ext_environment.py
	zip staging/Excel.zip	   vocola_ext_Excel.py
	zip staging/keys.zip       vocola_ext_keys.py \
				   SendInput.py \
                                   ExtendedSendDragonKeys.py
	zip staging/subprocess.zip vocola_ext_subprocess.py vocola_ext_variables.py
	zip staging/variable.zip   vocola_ext_variables.py
	zip staging/vocola.zip     vocola_ext_vocola.py
	zip staging/all_extensions.zip ${ALL}
	#
	zip -j staging/Win32Pad_0.1.1.zip  Win32Pad_0.1.1/*.vcl
	zip -j staging/Win32Pad_0.2.1.zip  Win32Pad_0.2.1/*.vcl \
					   Win32Pad_0.2.1/*.vch \
                                           vocola_ext_clipboard.py \
                                           vocola_ext_variables.py
	#
	for i in [A-Za-z]*.haml ; do \
	    ruby render.rb  $$i > staging/$${i%.haml}.html; \
	done

staging:
	mkdir staging

clean::
	rm -rf staging
