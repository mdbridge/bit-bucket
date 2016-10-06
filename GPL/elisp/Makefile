default: alist-data.el compile patches


alist-data.el: ~/Rmail/*/*  ~/mail/analysis/generate_alist.rb
	ruby ~/mail/analysis/generate_alist.rb > $@

clean::
	rm -f alist-data.el



compile: compile-24  #alist-data.el

compile-all: #alist-data.el
	-emacs -batch -f batch-byte-compile *.el

clean::
	rm -f *.elc



compile-22:                  # needed to run Emacs22...
	rm -f *.elc
	# only compile here code needed for Emacs 22:
	# csharp-mode doesn't work when byte compiled
	-ls *.el \
	    | grep -v csharp-mode.el \
	    | grep -v my-rmail23.el \
	    | grep -v my-rmail24 \
	    | grep -v vertical-motion-patch.el \
	    | grep -v wb-line-number-patched.el \
	    | xargs emacs22 -batch -f batch-byte-compile

compile-23: 
	rm -f *.elc
	# only compile here code needed for Emacs 23+:
	# csharp-mode doesn't work when byte compiled
	-ls *.el \
	    | grep -v csharp-mode.el \
	    | grep -v my-rmail24 \
	    | grep -v linum-22.el \
	    | grep -v ruby-mode.el \
	    | grep -v vertical-motion-patch.el \
	    | grep -v wb-line-number-patched.el \
	    | xargs emacs23 -batch -f batch-byte-compile

compile-24:
	rm -f *.elc
	# only compile here code needed for Emacs 24:
	# csharp-mode doesn't work when byte compiled
	-ls *.el \
	    | grep -v csharp-mode.el \
	    | grep -v my-rmail23.el \
	    | grep -v ls-lisp-patch.el \
	    | grep -v linum-22.el \
	    | grep -v ruby-mode.el \
	    | grep -v vertical-motion-patch.el \
	    | grep -v wb-line-number-patched.el \
	    | xargs emacs24_3 -batch -f batch-byte-compile



## 
## 
## 

to_PC: compile
	rm -rf to_PC
	mkdir to_PC
	cp *.el *.elc to_PC/
	cp "PC emacs"/* to_PC/

clean::
	rm -rf to_PC
	rm -f "PC emacs"/*~


##
## 
##

patches:
	# "my version is modified to rename align-regexp with a prefix of steve"
	-diff align-regexp.el.orig align-regexp.el > align-regexp.el.patch
	#
	-diff wb-line-number.el.orig wb-line-number-patched.el > wb-line-number.el.patch

clean::
	rm -f align-regexp.el.patch wb-line-number.el.patch
