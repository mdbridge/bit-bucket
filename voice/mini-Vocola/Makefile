COMMANDS=commands
LISTS=lists


## 
## Mini-Vocola:
## 

to_DNS.xml: ${COMMANDS}/* ${LISTS}/*.list \
	    contexts/* extensions/* \
	    compile.rb compile_main.rb statements.rb
	ruby compile.rb ${COMMANDS}/*.mv ${LISTS}/*.list

# "import out" from Command Browser should load the resulting commands

clean::
	rm -f to_DNS.xml
	rm -f commands/*~ lists/*~ contexts/*~ extensions/*~
