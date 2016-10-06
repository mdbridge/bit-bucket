VCL = ~/voice/my_commands/commands/*.vch
VCH = ~/voice/my_commands/commands/*.vcl

MV  = ~/voice/my_commands/mini-commands/*.mv
MVH = ~/voice/my_commands/mini-commands/*.mvh

VC  = ${VCL} ${VCH}
MVC = ${MV}  ${MVH}


look_for_errors:
	python parser.py ${VC} test.vcl ${VCH} > /dev/null

parse_everything:
	python parser.py ${VC} test.vcl ${VCH} 

test:
	python parser.py < test.vcl


commands:
	python commands.py ${VC} test.vcl ${VCH} 



bar:
	for f in ~/voice/my_commands/commands/*; do \
		echo $$f; \
		echo ; \
		python extract.py < $$f ; \
	done





clean::
	rm -f *.pyc *~
	rm -f parser.out parsetab.py
