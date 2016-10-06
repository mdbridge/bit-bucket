all: home work

RUBY = ruby -E Windows-1252

HOME = home_Vocola_history*.txt


## 
## Basic folding of inputs:
## 

home.txt: home_Vocola_history*.txt   fold.rb 
	cat ${HOME} | ${RUBY} fold.rb > home.txt

home:
	make home_1.txt home_2.txt home_3.txt

clean::
	rm -f home.txt


work work.txt: work_Vocola_history*.txt   fold.rb
	cat work_Vocola_history*.txt | ${RUBY} fold.rb > work.txt
	make work_1.txt work_2.txt work_3.txt

clean::
	rm -f work.txt


combined combined.txt: home.txt work.txt  fold.rb
	cat home.txt work.txt > combined.txt
	make combined_1.txt combined_2.txt combined_3.txt

clean::
	rm -f combined.txt


## 
## Sorting by count:
## 

%_1.txt: %.txt count.rb
	(LC_ALL=C; export LC_ALL; sort $< | ${RUBY} count.rb 1 | sort -n -r > $@)

%_2.txt: %.txt count.rb
	(LC_ALL=C; export LC_ALL; sort $< | ${RUBY} count.rb 2 | sort -n -r > $@)

%_3.txt: %.txt count.rb
	(LC_ALL=C; export LC_ALL; sort $< | ${RUBY} count.rb 3 | sort -n -r > $@)


%_6.txt: %.txt count.rb
	(LC_ALL=C; export LC_ALL; sort $< | ${RUBY} count.rb 6 | sort -n -r > $@)

%_8.txt: %.txt count.rb
	(LC_ALL=C; export LC_ALL; sort $< | ${RUBY} count.rb 8 | sort -n -r > $@)

%_9.txt: %.txt count.rb
	(LC_ALL=C; export LC_ALL; sort $< | ${RUBY} count.rb 9 | sort -n -r > $@)



clean::
	rm -f *_[1-9].txt


## 
## Combining commands:
## 

%_s2.txt: %.txt  shingle.rb
	cat $< | ${RUBY} shingle.rb 2 > $@

%_s3.txt: %.txt
	cat $< | ${RUBY} shingle.rb 3 > $@

clean::
	rm -f *_s[1-9].txt


## 
## Frequency of command pairs and triples:
## 

pairs:
	make combined_s2_8.txt
	more combined_s2_8.txt

triples:
	make combined_s3_9.txt
	more combined_s3_9.txt


## 
## Sample commands at random:
## 

random: combined.txt
	shuf -n 100 < combined.txt | head -n 100

random_recent: home_Vocola_history.txt work_Vocola_history.txt
	cat home_Vocola_history.txt work_Vocola_history.txt > recent.txt
	shuf -n 100 < recent.txt | head -n 100

clean::
	rm -f recent.txt


## 
## Experiments:
## 

random-work-utterance:
	cat work_Vocola_history*.txt \
	  | sed 's/\\space-bar\\space bar/space bar/g' \
	  | sed 's/\\space-bar/space bar/g' \
	  | sed 's/,\\comma\\comma/,/g' \
	  | sed 's/,\\comma/,/g' \
	  | sed 's/:\\colon\\colon/:/g' \
	  | sed 's/:\\colon/:/g' \
	  | sed 's/;\\semicolon\\semicolon/;/g' \
	  | sed 's/;\\semicolon/;/g' \
	  | sed 's/\.\\period\\period/./g' \
	  | sed 's/\.\\period/./g' \
	  | sed 's/\.\\dot\\dot/./g' \
	  | sed 's/\.\\point\\point/./g' \
	  | sed 's/\.\\point/./g' \
	  | sed 's|/\\slash\\slash|/|g' \
	  | sed 's|/\\slash|/|g' \
	  | sed 's/\?\\question-mark/?/g' \
	  | sed 's/_\\underscore\\underscore/_/g' \
	  | sed 's/_\\underscore/_/g' \
	  | sed 's/-\\hyphen\\hyphen/-/g' \
	  | sed 's/-\\hyphen/-/g' \
	  | sed 's/"\\left-double-quote\\open quote/"/g' \
	  | sed 's/"\\open-quote/"/g' \
	  | sed 's/"\\right-double-quote\\close quote/"/g' \
	  | sed 's/"\\close-quote/"/g' \
	  | sed 's/(\\left-parenthesis\\open paren/(/g' \
	  | sed 's/(\\open-paren/(/g' \
	  | sed 's/(\\left-parenthesis\\left paren/(/g' \
	  | sed 's/)\\right-parenthesis\\close paren/)/g' \
	  | sed 's/)\\close-paren/)/g' \
	  | sed 's/a\\determiner/a/g' \
	  | sed 's/I\\pronoun/I/g' \
	  | sed 's/one\\one/one/g' \
	  | sed 's/\\no-space\\no space/\\no-space/g' \
	  | sed 's/\\new-line\\new line/new line/g' \
	  | sed 's/\\new-paragraph\\new paragraph/new paragraph/g' \
	  | sed 's/\\cap\\cap/\\cap/g' \
	  > utterances.txt
	cat utterances.txt | shuf -n 100 > sample.txt
	cat utterances.txt | shuf -n 100 | head -n 100

clean::
	rm -f utterances.txt sample.txt
