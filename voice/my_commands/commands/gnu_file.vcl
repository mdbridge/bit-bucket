### 
### Commands for dealing with files
### 
### Also see: gnu_buffer.vcl
### 

include "gnu.vch";

include "extended_string.vch";
include "letters_plus.vch";
include "directories.vch";
include "files.vch";
include "projects.vch";


##
## Visiting files
##
##   Note: switches to an existing buffer if file already visited
##

find           file           = {ctrl+x}{ctrl+f}; 
find           file literally = Do(find-file-literally);
  # kill current buffer then find user-specified file:
find alternate file           = {ctrl+x}{ctrl+v};

# 
# find one from a predefined list:
# 
buffer <file>	     = FindFile(UNIX( $1{enter} ));
buffer <file> end    = FindFile(UNIX( $1{enter} )) {esc}>;

<paper> <paper_part> = FindFile(UNIX( $1/$2.tex{enter} ));


#
# find a file in specified directory:
#
find [<D>] <UNIX> [/ <COM>] [<last>] = FindFile(UNIX($1$2 When($3,/$3) When($4,$4,/)));
find       <PC>   [/ <COM>] [<last>] = FindFile(PC  (  $1 When($2,/$2) When($3,$3,/)));


# 
# find one from buffer contents:
# 
  # find file at point was poorly recognized:
find (this|that) file =     Do(find-file-at-point) {enter}; 
find file <row> <r>   = LineMod($2) Leap(d, ".") 
                            Do(find-file-at-point) {enter};

# 
# related file:
# 
other file	      = Do(ff-find-other-file);


##
## Insert file at point, optionally from another specified directory:
##

Insert(prefix) := {ctrl+x}i EraseToStart() $prefix;

insert file	   = {ctrl+x}i Empty();  # no erase prefix
insert from <UNIX> = Insert(UNIX(  $1      /));



### 
### Dired mode:
### 

Mondo help    = h;   # pull down menus also useful
Mondo refresh = g;


## 
## Visiting a directory with dired mode:
## 

## Entire directories:

# "find XYZ slap" enters dired mode for directory XYZ

  # Mondo for directory containing current file/directory:
Mondo here  = Do(dired-jump);

  # this does not replace current buffer:  <<<>>>
Mondo (parent|go up) = Do(dired-jump);


## Subset of files of a directory:

  # {ctrl+x}d path/a* gives wildcard dired buffer, seperate from normal one
DirEdit(directory, pattern) :=
	FindFile($directory {enter})
	U g  # remove any existing restriction, marks
	%m $pattern {enter} tk
	{up_2}{m_2}k;    # kill "." and ".." lines...

Mondo here <letterp>		 = DirEdit("", "^[$1" Capitalize($1) "]");

Mondo       restrict <_anything> = DirEdit("", $1);
find <UNIX> restrict <_anything> = DirEdit(UNIX($1), $2);
  # return to entire directory:
Mondo unrestrict		 = g;

Mondo monster <_anything>	 = DirEdit("", \< Symbols.Monster($1)\>);


## Subset of files of a directory tree:

StartMondo(directory) :=
    FindFile($directory) {enter}
    {ctrl+x}k{enter}                # remove any existing restriction
    FindFile($directory) {enter}
    {up_2} mm;
DoMondo(opens) :=
    Replace3(    
      Mid(Replace2($opens!, "<<", "!<<", ">>", "!>>"),1),
      ">>", {esc}< {ctrl+u}{ctrl+s},
      "<<", {esc}> {ctrl+u}{ctrl+r},
       "!", $ {enter} i {up_2}mm);
FinishMondo() := k {esc}<;  # remove . and ..'s

MondoWhere(instructions) :=
    StartMondo(Split($instructions, ",", 0))
    DoMondo(   Split($instructions, ",", 1))
    FinishMondo();

find <mondo_where> slap = MondoWhere($1);
  # if this matches a directory entry (not listing), you get all its children:
<mondo_where> monster <_anything> = 
    MondoWhere($1) %m \< Symbols.Monster($2)\> {enter} tk;


## 
## Opening a file:
## 

  # select a file of a dired buffer to display, killing the dired buffer
  # in the process:
Mondo <r>         = LineMod($1) a;
  # ditto but select file in Rmail mode:
Mondo <r> Rmail   = LineMod($1) a Do(rmail-mode);

  # open given file in Windows: <<<>>>
Mondo <r> Windows = LineMod($1) 
    Clipboard.Set("") {ctrl+u}0w Clipboard.WaitForNew("", 10)
    Variable.Set(":temp", Clipboard.Get())
    Clipboard.Set("")
    Do2(mdl-local-pathname-to-PC-pathname, Variable.Get(":temp"))
    Clipboard.WaitForNew("", 10)
    AppBringUp(Clipboard.Get(), Clipboard.Get());

Mondo phrase <_anything> = DirEdit("", $1) a;
        # later: only go to file if only one match?  <<<>>>
	# error: show all mondo files + error message?
#	Elisp('(setq mdl-buffer (current-buffer))') {enter}
#	Elisp('(kill-buffer mdl-buffer)');
  # <<<>>>
find <UNIX> phrase <_anything> = DirEdit(UNIX($1), $2) a;


## 
## Standard operations on files:
## 
## These operate on:
##   <prefix> files starting from file point is on if <prefix> present
##   all marked files if any are present
##   otherwise, the file under the point
## 

  # if multiple files, asks for destination directory; 
  # also renames visiting buffers
Mondo (rename|move)          = R Empty();
  # rename files being operated upon matching regexp to new filename
  # can use \&, \1, etc. in new name
Mondo rename [by] pattern    = %R;

  # if multiple files, asks for destination directory; preserves times, symlinks:
Mondo copy                   = C Empty();

  # delete marks for deletion, use expunge to actually delete:
Mondo delete                 = d Empty();
Mondo expunge                = x Empty();

Mondo (gzip|gunzip|compress) = Z;

# elsewhere: Mondo {search, search-and-replace}

  # use * surrounded by whitespace for list of all marked files
  # use ? surrounded by whitespace for 1 file at a time (like {} in find)
  # neither		     = ? at end
Mondo shell [command]	     = ! Empty();

Mondo mark		     = m;
Mondo unmark		     = u;  # exception: ignores set of marked files


## 
## Nonstandard operations that act directly on multiple files:
## 

EachLine(command, from, to) :=
	SaveExcursion(
          LineMod($from) 
  	  "{ctrl+x}(" $command "{ctrl+x})"
  	  {down}{home} Mark()
  	  LineMod($to) 
  	  Do(apply-macro-to-region-lines)
	);

Mondo delete <r> comma <r>   = EachLine(d{up}, $1, $2); # <<<>>>


  # mark all files whose name matches a regexp:
Mondo mark [by] pattern = %m;

MarkPattern(pattern) := %m $pattern {enter};

<kind> := ( C.     = MarkPattern(\.h$) MarkPattern(\.c$) MarkPattern(\.cpp$) 
          | Ruby   = MarkPattern(\.rb$)
          | Python = MarkPattern(\.py$)
          | Vocola = MarkPattern(\.vcl$) MarkPattern(\.vch$) 
          );

Mondo mark <kind> files = $1;
Mondo unmark all	= U;


## 
## Miscellaneous commands:
## 

  # this actually a toggle; use {ctrl+u}s to directly set ls flags:
Mondo (arrange|sort) by (name|date [modified]) = s;

Mondo set switches = {ctrl+u}s; # <<<>>>
Mondo show [(all=A)] recursively = {ctrl+u}s EraseToStart() -lR$1{enter}; # <<<>>>


Mondo new folder = + Empty();

Mondo clean up   = '%m~$'{enter}  '%m^#'{enter}  {esc}> D;

#
# Note: can toggle write protect, edit, then ^c^c to do mass renaming
#
# In Emacs 22, need Immediate->edit file names to start
#
