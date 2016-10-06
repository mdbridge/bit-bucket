### 
### Commands for managing Gnuemacs buffers
### 
### Also see: gnu_buffer_short.vcl, gnu_file.vcl
### 

include "gnu.vch";
include "extended_string.vch";

include "letters_plus.vch";


##
## For visiting files (switches to an existing buffer if file already
## visited), see gnu_file.vcl
##


##
## Switching between existing buffers
##

BuryMenu() := Elisp('(bury-buffer "*Buffer List*")');

switch-to-buffer = {ctrl+x}b;

<my_buffers> := ( shell = *shell*   | reply = *mail{tab} 
	        | Makefile	    | empty = *empty*
	        | grep  = *grep*    | compilation = *compilation* 
		| help = *Help*	    | messages = *Messages* 
		| Python = *Python* );

<my_buffers>                   buffer = {ctrl+x}b $1 {enter};

  # this is per window unlike most recently accessed commands below:
(next={right}|previous={left}) buffer = {ctrl+x}$1;


#
# Switching to a buffer by how recently it was switched to:
#

Other Buffer = {ctrl+x}b{enter};

  # go to Nth most recently accessed buffer:
<buffer_ord> := (
       third   = 3
     | fourth  = 4
     | fifth   = 5
     | sixth   = 6
     | seventh = 7
     | eighth  = 8
     | ninth   = 9
);

<buffer_ord> buffer = Do(mdl-recent-buffer-menu)
                      LineMod(Eval($1+0))  # +2 for Emacs V21...
                      f BuryMenu();

not  that buffer    = Do(bury-buffer)  {ctrl+x}b{enter};
kill that buffer    = {ctrl+x}k{enter} {ctrl+x}b{enter};


# 
# select most recent buffer whose name starts with <given>
# 

SelectRecent(prefix) := Do(mdl-recent-buffer-menu) {esc}<
			LeapRegex(D, '^....$prefix') Wait(200)
                        f BuryMenu();

letter <letterp> [<letterp>] buffer = SelectRecent($1$2);

not that <letterp> buffer = Do(bury-buffer) SelectRecent($1);

buffer phrase <_anything> =                 SelectRecent("[^0-9]*$1");


# 
# selecting designated buffers:
# 

<color> := ( red | orange | yellow | blue | green | white | black );

set <color> buffer = Elisp('(setq mdl-$1-buffer (current-buffer))');
    <color> buffer = Elisp('(switch-to-buffer mdl-$1-buffer)');


#
# Switching by menu choice:
#

  # bring up a buffer menu for buffer selection:
  # (for housecleaning, 'k'=mark for deletion, 'x'=purge, '?'=help)
recent [buffer] menu = Do(mdl-recent-buffer-menu) {esc}<;
       [buffer] menu = Do(mdl-buffer-menu); # <<<>>>

RestrictRecent(prefix) := Do(mdl-recent-buffer-menu) {esc}<
			 {ctrl+x}{ctrl+q}
			 {esc}< Do(keep-lines) '$prefix{enter}'
			 {ctrl+x}{ctrl+q};

  # same but show only buffers starting with $1:
buffer menu <letterp>		 = RestrictRecent('^....$1');

  # <<<>>>
buffer menu restrict <_anything> = RestrictRecent($1);


  # after buffer menu, go to buffer displayed on line r:
buffer <r> = LineMod($1) {enter} BuryMenu();


##
## Writing out buffers
##

  # save the contents of current buffer:
save     file       = {ctrl+x}{ctrl+s};
  # save all modified file-visiting buffers:
save all files      = {ctrl+u} Do(save-some-buffers);

TouchNSave          = {space}{backspace} {ctrl+x}{ctrl+s};

  # write out current buffer to a new file:
write file	    = {ctrl+x}{ctrl+w};

  # write current buffer to ~/Tmp/l1:
export buffer       = Do(mdl-export-buffer-to-l1);

  # diff current buffer against its file:
show buffer changes = Do(diff-buffer-with-file) {enter};


## 
## Printing buffers:
## 

  # characters always interpreted as Latin-1:
print buffer          = Do(mdl-export-buffer-to-l1)
#                        Shell("enscript -P\$PRINTER ~/Tmp/l1");
                        Shell("enscript "
			      IfHome("", "-DDuplex:true ")
			      "-P\$PRINTER ~/Tmp/l1");

  # non-Latin-1 buffer characters show up as question marks:
print buffer in color = Do(ps-print-buffer-with-faces);

  # non-Latin-1 buffer characters show up as question marks:
save as PDF = Clipboard.Set("")
              Do2(mdl-local-pathname-to-PC-pathname, ~/Tmp/buffer.pdf)
              Shell("rm ~/Tmp/buffer.ps")
              {ctrl+u} Do(ps-print-buffer-with-faces)
	      Wait(10) # Xming bug... <<<>>>
	      EraseToStart() ~/Tmp/buffer.ps {enter}
	      Wait(10) # Xming bug... <<<>>>
	      Shell("ps2pdf ~/Tmp/buffer.ps ~/Tmp/buffer.pdf")
	      Clipboard.WaitForNew("", 10)
	      Variable.Set("buffer:saved_PDF", 
	          # convert from PC back to UNIX:
	          Replace3(Clipboard.Get(), "w:", "work:",
		  		            "p:", "foil:",
					    "\", "/"))
	      ;


  # non-Latin-1 buffer characters show up as question marks:
remote print = 
              Shell("rm ~/Tmp/buffer.ps")
              {ctrl+u} Do(ps-print-buffer-with-faces)
	      Wait(10) # Xming bug... <<<>>>
	      EraseToStart() ~/Tmp/buffer.ps {enter}
	      Wait(10) # Xming bug... <<<>>>
	      Shell("ps2pdf ~/Tmp/buffer.ps ~/Tmp/buffer.pdf")
	      Shell("scp ~/Tmp/buffer.pdf mdl@ts-rhel7.labs.hpecorp.net:~/Tmp/")
	      Shell("ssh mdl@ts-rhel7.labs.hpecorp.net 'lpr ~/Tmp/buffer.pdf'")
	      ;


##
## Removing buffers
##

kill buffer = {ctrl+x}k{enter};

  # put current buffer at end of list of buffers:
bury buffer = Do(bury-buffer);


##
## Altering various buffer properties
##

write protect   = {ctrl+x}{ctrl+q};           # toggles

toggle truncate = Do(toggle-truncate-lines);

  # hide lines with indentation more than $1 characters:
selective display 0..20 = {esc}$1{ctrl+x}$;


# 
# Set (minor) mode of buffer; many of these are toggles...
# 
auto fill        mode = Do(auto-fill-mode);
auto revert tail mode = Do(auto-revert-tail-mode);
column number    mode = Do(column-number-mode);
font lock        mode = Do(font-lock-mode);
visual line      mode = Do(visual-line-mode)
                        Elisp('(setq line-move-visual nil)');
Makefile         mode = Do(makefile-mode);
overwrite        mode = Do(overwrite-mode);
shell script     mode = Do(shell-script-mode);
show paren       mode = Do(show-paren-mode);

  # <<<>>>
show paren       mode (one=parenthesis|two=expression) = 
    Do(show-paren-mode) Elisp("(setq show-paren-style '$1)");


# 
# The buffer character encoding:
# 

describe current (coding system|encoding) = Do(describe-current-coding-system);

<encoding> := (DOS=dos | PC=dos | Unix=unix);

use       <encoding> encoding = {ctrl+x}{enter}f $1 {enter};
revert to <encoding> encoding = {ctrl+x}{enter}r $1 {enter};


##
## Miscellaneous buffer management operations
##

  # killing original buffer also kills its clones:
clone buffer = {ctrl+x}4c;

please revert buffer = Do(revert-buffer) yes{enter};
undo buffer          = Do(revert-buffer) yes{enter};

  # rename buffer (leaves associated file unchanged)
rename buffer          = Do(rename-buffer);

  # rename a file (leaves any associated buffers unchanged)
rename file            = Do(rename-file);

rename file and buffer = Do(rename-file-and-buffer);

  # move current file to a different directory:
move file directory    = Do(move-buffer-file);

copy buffer (filename={ctrl+u}|pathname="") = $1 Do(mdl-copy-buffer-filename);


reopen that in (wordpad|notepad|PC Emacs=PF32() "\Emacs\bin\runemacs.exe"
       	       |water pad=PCfromPC(~pf32/Win32Pad/win32pad.exe)) = 
        Clipboard.Set("")
        Elisp('(mdl-local-pathname-to-PC-pathname)')
	Clipboard.WaitForNew("", 10)
        AppBringUp($1, "$1 " Clipboard.Get());
