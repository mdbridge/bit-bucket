default: input_commands convert



## 
## Input Vocola 2 code:
## 

.PHONY: input_commands

input_commands:
	rm -rf input_commands
	mkdir input_commands
	#
	cp ~/Tmp/redacted/notepad*.vcl input_commands/
	cp ~/Tmp/redacted/_[a-zA-Z]*.vcl input_commands/

foo:
	cp ~/voice/my_commands/to_Vocola/*.vch input_commands/ 
	#
	cp ~/voice/my_commands/to_Vocola/*.vcl input_commands/ 
	#cp ~/voice/my_commands/to_Vocola/win32pad*.vc[hl] input_commands/ 
	#cp ~/voice/my_commands/to_Vocola/emacs*.vc[hl] input_commands/ 
	#
	rm -f input_commands/_test_extensions@mdl.vcl
	rm -f input_commands/_Unimacro_regression.vcl
	cp replacements/*.vc[hl] input_commands/ 

clean::
	rm -rf input_commands



## 
## Converting to Mini Vocola from Vocola 2:
## 

convert: converter.py original_extensions.csv #input_commands
	rm -rf commands lists
	mkdir  commands lists
	cat original_extensions.csv new_extensions.csv > extensions.csv
	(COMPUTERNAME=mdl; export COMPUTERNAME;	\
	   python converter.py -extensions extensions.csv -f \
	   -log_stdout input_commands commands)

clean::
	rm -rf commands lists extensions.csv



## 
## Further converting to Advanced Scripting via Mini Vocola:
## 

HERE = $(shell pwd)
MINIVOCOLA = ~/voice/mini-Vocola

advanced-scripting: convert
	rm -f ${MINIVOCOLA}/to_DNS.xml
	(cd ${MINIVOCOLA}; \
	 make "COMMANDS=${HERE}/commands" "LISTS=${HERE}/lists")
	cp ${MINIVOCOLA}/to_DNS.xml .

clean::
	rm -f to_DNS.xml



## 
## Sampling commands:
## 

all.mv global.mv xwin.mv: input_commands convert
	cat commands/*.mv > all.mv
	cat commands/_*.mv > global.mv
	cat commands/xwin*.mv > xwin.mv
	wc -l all.mv global.mv xwin.mv

samples: all.mv global.mv xwin.mv
	shuf -n 100 < all.mv > sample_all.mv
	shuf -n 100 < global.mv > sample_global.mv
	shuf -n 100 < xwin.mv > sample_xwin.mv

clean::
	rm -f all.mv global.mv xwin.mv
	rm -f sample_all.mv sample_global.mv sample_xwin.mv
