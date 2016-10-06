default: convert


.PHONY: input_commands

input_commands:
	rm -rf input_commands
	mkdir input_commands
	cp ~/voice/my_commands/to_Vocola/*.vc[hl] input_commands/ 
	rm input_commands/_test_extensions@mdl.vcl
	cp replacements/*.vc[hl] input_commands/ 

clean::
	rm -rf input_commands


convert: input_commands original_extensions.csv new_extensions.csv converter.py
	rm -rf output_commands
	mkdir output_commands
	cat original_extensions.csv new_extensions.csv > extensions.csv
	(COMPUTERNAME=mdl; export COMPUTERNAME;	\
	   python converter.py -extensions extensions.csv -f \
	   -log_stdout input_commands output_commands)
	#
	echo '"import Vocola 3 commands" should transfer the commands'

clean::
	rm -rf output_commands extensions.csv
