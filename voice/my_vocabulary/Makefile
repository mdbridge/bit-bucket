default: clean compile-all


## 
## Automatic generation of some vocabulary files:
## 

source::

-include Makefile.local


# 
# Symbol vocabulary is specified using a concise specification:
# 
source:: symbols.txt

symbols.txt: vocabulary/symbols.in   generate_symbols.rb
	ruby generate_symbols.rb < vocabulary/symbols.in > $@

clean::
	rm -f symbols.txt



##
## Collecting list of all vocabulary words:
## 

all.txt: source vocabulary/*.txt symbols.txt
	cat vocabulary/*.txt symbols.txt > all.txt

clean::
	rm -f all.txt


##
## Generating an incremental list of vocabulary words not already loaded:
## 

new.txt: all.txt already_loaded.txt
	sort all.txt            | grep -v '^#' > /tmp/all_sorted.txt
	sort already_loaded.txt | grep -v '^#' > /tmp/loaded_sorted.txt
	comm -23 /tmp/all_sorted.txt /tmp/loaded_sorted.txt > new.txt

clean::
	rm -f new.txt



## 
## Preparing for loading vocabulary words:
## 

compile-all: all.txt
	cp all.txt input.txt
	make compile

compile-new: new.txt
	cp new.txt input.txt
	make compile

clean::
	rm -f input.txt


LOADER=~/voice/my_commands/commands/natspeak_input.vcl

compile: 
	ruby word_compiler.rb -only -avoid_bug input.txt > ${LOADER}
	ruby word_compiler.rb -DNS11 -avoid_bug input.txt > normal_words_list.txt
	@echo
	@echo '** "load normal vocabulary"'
	@echo
	@echo
	@echo "** After loading Vocola commands and disabling RSIGuard,"
	@echo "**"
	@echo "** say 'edit words, then 'load compiled words'"
	@echo

clean::
	rm -f ${LOADER} normal_words_list.txt



##
## Experiments:
## 

spelling:
	ruby combine.rb > input.txt
	make compile
