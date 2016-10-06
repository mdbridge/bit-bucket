-include Makefile.local


Vocola-prepare:
	rm -rf Vocola
	scp -r mdl@foil.strangled.net:~/voice/Vocola_development/Vocola_2/build/Vocola .
	@echo
	@echo "***** Hopefully, was on git branch Mark, make done"

clean::
	rm -rf Vocola

bootstrap-prepare:: Vocola-prepare
	(cd ~/elisp;               make to_PC)
	(cd ~/voice/my_vocabulary; make clean; make)
	(cd ~/voice/my_commands;   make clean; make source; make)
	(cd ~/voice/my_commands;   make corpus)
	scp 'mdl@foil.strangled.net:~/voice/DNS\ software/DPI\ 14\ setup.exe' PC_setup/
	@echo
	ls -ld Vocola
	grep version Vocola/README.html
	ls -l "PC_setup/DPI 14 setup.exe"

clean::
	#rm -rf PC_setup/DNS12ServicePack1.exe
	#rm -rf PC_setup/DNS1251UPD.exe
	#rm -rf PC_setup/DPI 14 setup.exe

bootstrap-print:
	enscript -P$$PRINTER ~/admin/bootstrap.txt
	enscript -P$$PRINTER ~/admin/browser-setup.txt
	enscript -P$$PRINTER ~/admin/voice-setup.txt
	enscript -P$$PRINTER ~/elisp/PC\ emacs/instructions.txt
	enscript -P$$PRINTER ~/voice/DNS\ serial\ numbers.txt



.PHONY: bootstrap

bootstrap: #bootstrap-prepare
	rm -rf bootstrap
	mkdir                                   bootstrap
	#
	cp ~/.cshrc                             bootstrap/.cshrc
	cp ~/.emacs                             bootstrap/.emacs
	#
	mkdir                                   bootstrap/.ssh
	chmod og=                               bootstrap/.ssh
	#
	#  fix Cygwin backspacing problem:
	sed '/delete-char/d' ~/.tcshrc        > bootstrap/.tcshrc
	cp ~/.Xdefaults                         bootstrap/.Xdefaults
	#
	mkdir                                   bootstrap/AutoHotkey
	cp ~/voice/my_commands/AutoHotkey/*     bootstrap/AutoHotkey/
	#
	mkdir                                   bootstrap/bin
	cp ~/bin/average_numbers                bootstrap/bin/average_numbers
	cp ~/bin/color_column.rb                bootstrap/bin/color_column.rb
	cp ~/bin/count_numbers                  bootstrap/bin/count_numbers
	cp ~/bin/fields                         bootstrap/bin/fields
	cp ~/bin/number_files                   bootstrap/bin/number_files
	cp start_Cygwin.bat                     bootstrap/bin/
	#
	mkdir                                   bootstrap/corpus
	cp ~/voice/my_commands/corpus/*         bootstrap/corpus/
	#
	mkdir                                  "bootstrap/Do Not Backup"
	mkdir                                  "bootstrap/Dragon user backups"
	#
	mkdir                                   bootstrap/elisp
	cp ~/elisp/to_PC/*                      bootstrap/elisp/
	#
	cp login_*.bat                          bootstrap/
	#
	mkdir                                   bootstrap/mail
	mkdir                                   bootstrap/mail/drafts
	#
	mkdir                                   bootstrap/NatLink
	mkdir                                   bootstrap/NatLink/Vocola
	cp ~/voice/my_commands/to_Vocola/*      bootstrap/NatLink/Vocola/
	#
	mkdir                                   bootstrap/scratch
	mkdir                                   bootstrap/setup
	#
	#
	cp ~/admin/bootstrap.txt                                       bootstrap/setup/
	cp ~/admin/home*.txt                                           bootstrap/setup/
	cp ~/admin/browser-setup.txt                                   bootstrap/setup/
	cp ~/backups/downloads_needed.html                             bootstrap/setup/
	cp -r ~/backups/PC_setup/*                                     bootstrap/setup/
	(cd bootstrap/setup; mkdir Emacs)
	(cd bootstrap/setup; mv emacs-24.5-bin-i686-mingw32.zip Emacs/)
	(cd bootstrap/setup/Emacs; unzip -q emacs-24.5-bin-i686-mingw32.zip)
	cp ~/admin/software/ExpanDrive/*License                        bootstrap/setup/
	cp ~/backups/Vocola-patches.bat                                bootstrap/setup/
	##
	mkdir                                                          bootstrap/setup/Cygwin64
	##
	cp -r ~/backups/Vocola                                         bootstrap/setup/Vocola
	##
	mkdir                                                          bootstrap/setup/voice
	cp ~/admin/voice-setup.txt                                     bootstrap/setup/voice/
	cp ~/voice/DNS\ serial\ numbers.txt                            bootstrap/setup/voice/
	cp ~/voice/my_vocabulary/normal_words_list.txt                 bootstrap/setup/voice/
	cp ~/voice/my_commands/Freesr/Mark.lang                        bootstrap/setup/voice/
	###
	mkdir                                                          bootstrap/setup/voice/advanced-scripting
	cp ~/voice/my_commands/advanced-scripting/*                    bootstrap/setup/voice/advanced-scripting/
	###
	mkdir                                                          bootstrap/setup/voice/extensions
	cp ~/voice/my_commands/extensions/*                            bootstrap/setup/voice/extensions/
	###
	mkdir                                                          bootstrap/setup/voice/NatLink
	cp ~/voice/my_commands/NatLink/*                               bootstrap/setup/voice/NatLink/
	###
	make bootstrap-local

clean::
	rm -rf bootstrap


bootstrap-local::

