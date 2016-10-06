default: translate

all: source translate

-include Makefile.local



## 
## Generating some Vocola lists automatically:
## 

source::
	(cd generators; ruby numbers.rb > ../commands/numbers.vch)
	#
	ruby generators/voice_source.rb \
	    natspeak_input.vcl \
	    numbers.vch \
	    command_files.vch \
            include_files.vch vocabulary_files.vch \
	    java_types.vch personal_emails.vch work_emails.vch \
	    units.vch interfaces.vch \
	    web_sites.vch work_sites.vch \
	    personal_folder.vch \
	    UNIX.vch PC.vch COM.vch

# these are ignored in work:~/.unison/commands.prf:
clean::
	rm -f commands/numbers.vch
	rm -f commands/command_files.vch 
	rm -f commands/include_files.vch commands/vocabulary_files.vch


source:: filesystem
	ruby generators/combine_scans.rb UNIX \
	    commands/filesystem/*_Unix_scan.txt > commands/UNIX.vch
	ruby generators/combine_scans.rb PC \
	    commands/filesystem/PC_directories.txt > commands/PC.vch
	ruby generators/combine_scans.rb -components COM \
	    commands/filesystem/components.txt \
	    commands/filesystem/*_Unix_scan.txt \
	    commands/filesystem/PC_directories.txt > commands/COM.vch

# these are ignored in work:~/.unison/commands.prf:
clean::
	rm -f commands/UNIX.vch
	rm -f commands/PC.vch
	rm -f commands/COM.vch



## 
## Prepare command files for export to Vocola:
## 

translate: #source
	ruby -E Windows-1252 translate.rb commands/*.vch
	ruby -E Windows-1252 translate.rb commands/*.vcl commands/*.vch

clean::
	rm -f to_Vocola/*



## 
## Generating a training corpus for DNS:
## 

.PHONY:: corpus

corpus:
	-rm -rf corpus
	mkdir corpus
	#
	make local-corpus
	#
	# loop over top-level Rmail directories:
	for p in `cd ~/Rmail; find . -maxdepth 1 -mindepth 1 -type d ! -name .svn -print | sed 's|^\./||'` ;do \
	    echo "scanning $$p..."; \
	    ruby ~/mail/analysis/extract_email.rb 240 \
		~/Rmail/$$p/* ~/Rmail/$$p/*/* >> corpus/mail_$$p.txt; \
	done

local-corpus::

clean::
	-rm -rf corpus
