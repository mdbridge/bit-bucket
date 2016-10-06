LOADER=~/voice/my_commands/commands/natspeak_input.vcl

default: import ${LOADER} all_dns11.txt

clean::
	rm -f *.pyc


## 
## Automatic generation of some vocabulary files:
## 

GENERATED_WORDS= vocabulary/symbols.txt
#GENERATED_WORDS=

vocabulary/symbols.txt: vocabulary/symbols.in   generate_symbols.py
	python generate_symbols.py < vocabulary/symbols.in > $@

clean::
	rm -f ${GENERATED_WORDS}



## 
## Compiling/loading list of all vocabulary words:
## 

all.txt: vocabulary/*.txt ${GENERATED_WORDS}
	cat vocabulary/*.txt > all.txt

clean::
	rm -f all.txt

import: all.txt
	@echo
	@echo "** From a MS-DOS command-line window speak:"
	@echo "**"
	@echo '** "load all vocabulary"'


all_words.txt: all.txt    word_format.py 
	python word_format.py all.txt | sort - > $@

clean::
	rm -f all_words.txt


all_dns11.txt: all.txt
	cat all.txt \
	| egrep -v ':(open|close) (quote|paren|bracket|brace|single quote)' \
	| egrep -v ':(left|right) (paren|bracket|brace)' \
	> $@



${LOADER}: all.txt   prepare_load_properties.py
	python prepare_load_properties.py all.txt > $@

clean::
	rm -f ${LOADER}



## 
## Words present in DNS, but not accounted for by vocabulary/*, crap.txt:
## 

  # export DNS vocabulary to this file:
#DNS_WORDS=exported/*_exported.txt
DNS_WORDS=exported/home7_exported.txt
#DNS_WORDS=exported/work2_exported.txt

ACCOUNTED_DEFNS= vocabulary/*.txt ${GENERATED_WORDS}
ACCOUNTED_WORDS= crap.txt

unaccounted: unaccounted.txt unaccounted_spoken.txt unaccounted_phrases.txt \
	unaccounted_lowercase.txt unaccounted_capitalized.txt \
	unaccounted_leading.txt


unaccounted.txt: ${ACCOUNTED_DEFNS} ${ACCOUNTED_WORDS} ${DNS_WORDS}
	cat ${ACCOUNTED_DEFNS} | python word_format.py - > tmp.0
	cat tmp.0 ${ACCOUNTED_WORDS} | sort -u - > tmp.1
	cat ${DNS_WORDS} | tr -d '\r' | sort -u - > tmp.2
	#comm -3 tmp.1 tmp.2
	comm -1 -3 tmp.1 tmp.2 > $@
	rm -f tmp.?

clean::
	rm -f tmp.? unaccounted*.txt


unaccounted_spoken.txt: unaccounted.txt
	- grep '\\' unaccounted.txt > $@

unaccounted_phrases.txt: unaccounted.txt
	- grep -v '\\' unaccounted.txt \
	| grep ' ' > $@

unaccounted_lowercase.txt: unaccounted.txt
	- grep -v '\\' unaccounted.txt \
	| grep -v ' ' \
	| grep -v '[A-Z]' > $@

unaccounted_capitalized.txt: unaccounted.txt
	- grep -v '\\' unaccounted.txt \
	| grep -v ' ' \
	| grep '[A-Z]' > $@


unaccounted_leading.txt: unaccounted_capitalized.txt
	- grep '^[A-Z]' unaccounted_capitalized.txt | grep -v '^..*[A-Z]' > $@

