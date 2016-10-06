###
### Voice commands for writing programs in Emacs
###
###   Language specific commands are in subfiles.
###

include "gnu.vch";
include "optional.vch";


###
### Inserting comments:
###

# see [un]comment <range>; gives {home}// for C++, but /* ... */ for C


  # Form of /*...*/:
comment pair <_anything> = /* When($1,$1*/,*/{left_2} Empty());


##
## Form of X\nX\nX:
##

<comment_character> := ( pound = "#" | ';' | semi = ";" | slash = / 
	               | apostrophe = "'" | bang = "!" | star = "*"
		       | percent = "%" | remark = REM );

start [(double=2|triple=3)] <comment_character> comment = 
    {home}{ctrl+o}
    Repeat(2, REPEAT($1,$2) {enter})  REPEAT($1,$2)
    {up}{end} " ";
      

##
## Form of /X\nX\nX/
##

<comment_type> := ( comment = "*" | description = "**" );

# 
# C++ mode (but not Java mode) needs the space before /* to indent the
# comment properly...
# 
start <comment_type>       = {home}{ctrl+o}" "/$1{tab}{enter}
                                               $1{enter}
                                               $1/             {up}{end}{space};

more <comment_type> [2..9] = REPEAT($2, {enter}{tab}$1{space} );

[<row> <r>] open comment [2..20] = When($1,LineMod($2)) REPEAT($3, {ctrl+o}*{tab});

jump <comment_type>        = Leap(D, $1/) {down}{home}{tab};


## 
## Form of a box:
## 

Box(start, middle, end) :=
              $start {ctrl+u}73$middle $end {enter}
    Repeat(3, $start {ctrl+u}73{space} $end {enter})
              $start {ctrl+u}73$middle $end {enter}
    {up_3} Leap(d+," ") {backspace}" ";

start <comment_character> box = Box($1, $1, $1);
start comment             box = Box(/*, *, */);



### 
### Make temporary copy of a line for alteration leaving original commented out:
### 

<symbol> := ( pound = '#' | ';' | semi = ';' | '%' | percent = '%' | slash = '//' 
	    | bang = '!');

Alter(prefix) := {home} $prefix Mark() {end}{esc}w
	      	 {home}{down}{ctrl+o} {ctrl+y} {home};

alter           line [<symbol>] = Alter(When($1,$1,{ctrl+q}/{ctrl+q}/));
alter indented  line		= Alter({tab}//) {tab};
alter indented [line] <symbol>	= Alter({tab}$1) {tab};



###
### Symbols:
###

Compile(command) := {ctrl+u} Do(compile) EraseToStart() $command {enter};

compile symbols       = Compile("(cd ~/voice/symbols; make)");


# toggle is a range command

toggle start = Mark() {home} ToggleRegion();


type Monster <_anything> = Mark() "monster " $1 ToggleRegion() {ctrl+x}{ctrl+x};



###
### Tags commands:
###

load tags      = Do(visit-tags-table);
load tags here = Do(visit-tags-table) {enter};

  # attempt to complete text around cursor using tags:
complete tag   = Do(complete-tag);

  # goto occurrence of a tag:
find tag       =         {esc}.;
next tag       = {ctrl+u}{esc}.;
#find tag other = {ctrl+x}4.;  # display tag in another window

# see teleport tag, tags search, locate tag in gnu_locate.vcl
# see tags search-and-replace in gnu_repeated.vcl



### 
### Converting programming style by adding spaces:
### 

<loosen> := (       loosen=Do(query-replace-regexp) 
	    | Mondo loosen=Q
            );

Loosen(operator, from, to) := $operator
		       	      Replace($from, '\s', 
			      		     ' {ctrl+q}{ctrl+j}{ctrl+q}{ctrl+i}')
			      {enter} $to {enter};


<loosen> braces     = Loosen($1, '\([^\s]\){', '\1 {');

  # user must be careful about: ;\ and ';'
<loosen> semicolons = Loosen($1, ';\([^\s;)]\)', '; \1');  # allow ;;)

  # loosen =, -=, +=, *=, |=
loosen operators = Do(query-replace-regexp) 
                  '\([^=! <>]\)\([-+*|]?=\)\([^=]\)' {enter} '\1 \2 \3' {enter};


SpaceAfter(character) := Do(query-replace-regexp) 
       		    '$character\([^ {ctrl+q}{ctrl+j}]\)' {enter} 
		    '$character \1' {enter};

loosen (commas=',') = SpaceAfter($1);



### 
### Miscellaneous:
### 

  # I use this to indicate code that needs more work; not a valid word
kluge mark = "<<<>>>";   

  # move forward to start of next "defun"; works in many modes:
start function [1..10] = {ctrl+u}- When($1,$1,1) {esc}{ctrl+a};
  # ditto but backwards:
back  function [1..10] = {ctrl+u}  When($1,$1,1) {esc}{ctrl+a};

## 
## Keeping access to these around for now as convenient for testing:
## 

include "java_types.vch";

#  switch-to-buffer/find file for my given java type:
elephant buffer <java_type> = 
        FindFile(UNIX("work:~/deduplication/client/src/$1.java") {enter});
