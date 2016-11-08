###
### Commands for Unix command shells (including via Gnuemacs *shell* buffer)
###
### See also UNIX_item.vcl
###

include "numbers.vch";
include "letters.vch";
include "string.vch";
include "optional.vch";


PLACE(before, after) := $before $after {left_ Len($after) };

SHELL(pathname) := Replace($pathname, " ", "\ ");


### 
### Issuing new commands:
### 

## 
## General shell commands:
## 

include "any_shell.vch";


## 
## Commands for navigating around filesystems
## 

CD(pathname)   := 'CD ' SHELL($pathname) {enter} Empty();
DOWN(pathname) := CD($pathname);
UP(count)      := CD('.' Repeat($count, UNIX(/..)));


include "navigate.vch";


MOVE(pathname) := 'cd ' SHELL($pathname) {enter} Empty();

quietly move to <UNIX> [/ <COM>] = MOVE(UNIX($1 When($2,/$2)));


TYPE(pathname) := SHELL($pathname);

type directory  <UNIX> [/ <COM>] = TYPE(UNIX($1 When($2,/$2)));


# 
# Designating directories for later return:
# 

<place_color> := ( red | blue | green | yellow | white | black | orange );

set <place_color> place = 
    'rm -rf ~/Tmp/places/$1; ln -s "`pwd`" ~/Tmp/places/$1'{enter};

show places = 'ls -l ~/Tmp/places'{enter};


# 
# A simple directory stack:
# 

show stack = 'dirs' {enter};

push place = 'pushd `pwd`' {enter};
pop  place = 'popd'        {enter};
next place = "pushd +1{enter}";


##
## Directory information/listing commands
##

<show> := ( show          = "" 
          | show some of  = " | head" 
          | show a lot of = " | head -n 50"
	  | local show    = {ctrl+a}l{ctrl+e}  # for use with sftp
	  );

<desc> := (long = -l | full = -A | recent = -t | recursive = -R | human = -h);

<show> [<desc> [<desc> [<desc>]]] directory = 
    ls When($2," $2") When($3," $3") When($4," $4") $1{enter};


(show | print) working directory = "pwd{enter}";


show disk space = "df -h .{enter}" Empty();

show 	    [(medium="-t 1M"|large="-t 1G")]         disk usage = 
    "du " When($1,$1,"") " -s -c -- ""`ls --color=none -A`"" .{enter}" Empty();

show sorted [(medium="-BM -t 1M"|large="-BG -t 1G")] disk usage = 
    "du " When($1,$1,"-BK") " -s -- ""`ls --color=none -A`"" ."
      " | sort -n{enter}" Empty();


##
## Changing permissions:
##

# chmod commands are now in programming_shell.txt vocabulary

fix permissions  = fix_permissions{enter};

fix permissions recursively = 
    "find . -type d -print0 | xargs -0 -L 1 fix_permissions" {enter};


## 
## Macros for (s)ftp/ssh:
## 

  # make ssh connection to given machine, user pair:
secure login [to] <machine> = "ssh -X $1{enter}";


S F T P <machine> = "sftp $1{enter}";

# see local show ... directory above   (= lls ...)

multiple get            = "mget ";
local change directory  = "lcd ";

local working directory = lpwd{enter};


##
## Copying between machines using incoming and outgoing directories:
##

# Analogs for to/from PC in import.vcl

export to <machine> =
    "rsync -t --progress -z -p -r --ignore-times --delete "
    "~/Tmp/outgoing/ $1:~/Tmp/incoming"{enter};

import from <machine> =
    "rsync -t --progress -z -p -r --ignore-times --delete "
    "$1:~/Tmp/outgoing/ ~/Tmp/incoming"{enter};


## 
## Process control:
## 

<job> := ( job | process );

foreground <job> [1..10] = fg When($2," %$2") {enter};
background <job>         = bg     {enter};

  # show top processes, then prepare to kill some of them:
kill top processes = "top -n 1{enter}" Wait(1000) "kill -HUP ";

locate process phrase <_anything> = 'ps a | grep -i "$1"' {enter};


##
## Pager macros
##

bar more           = " |& more{Enter}";
bar less           = " |& less{Enter}";

bar top            = " |& head -n 30" {home}"clear; " {end}{Enter};
bar head <my0to99> = " |& head -n $1"{enter};


  # skip to next file in a pager:
pager next     = ":n{Enter}";
  # skip to previous file in a pager:
pager previous = ":p{Enter}";


## 
## Macros for starting xterms:
## 

include "switch.vch";

Font() := ' -fa "DejaVu Sans Mono" -fs 9';

<new_xterm> := ( new raw   xterm='xterm -geometry =80x42'
	       | new       xterm='xterm -geometry =80x42' Font()
	       | new tall  xterm='xterm -geometry =80x60' Font()
               | new large xterm='xterm -geometry =80x42'
	                           ' -fa "DejaVu Sans Mono" -fs 11'
               );

<new_xterm> <window_suffix> = 
	SwitchTo2("^xterm $2$", $1 ' -T "xterm $2" -n "xterm $2" &{enter}');

(xterm|set) title <window_suffix> = '\echo -e "\033]0;xterm $2\007"' {enter};
(xterm|set) title startup [xterm] = '\echo -e "\033]0;startup xterm\007"' {enter};


## 
## Macros for tar:
## 

tar (create|build) = "tar cvf ";


## 
## Commands for dealing with Linux packages:
## 

<operation> := ( search | show | install | remove );

aptitude <operation> = "sudo aptitude $1 ";

apt (search="apt-cache search" | update caches="apt-get update" 
    | install="apt-get install" | show="apt-cache show") = "$1 ";

Yum list              = "yum list | grep -i ";
Yum info              = "yum info ";
Yum install           = "sudo yum install ";
Yum install developer = PLACE("sudo yum install ", "-devel");

Yum what provides     = PLACE("yum whatprovides '*/", "'");


## 
## Command templates:
## 

  # next one really needs a no-space... <<<>>>
Unix find      command = PLACE("find . -name \*", "\* -print");
Unix find exec command = PLACE("find . -name \*", " -type f -exec {} \;");
Unix find grep command = PLACE("find . -type f -exec grep --color=always -H -i '", 
     	       	       	       "' {} \; -print") Empty();

Unix said command = PLACE("sed 's/", "/g' ");

Unix time it command 
     = PLACE('python -m timeit -v -r 10 "__import__(' "'os').system('", "')""");


## 
## Setting which printer to print on:
## 

include "printers.vch";

Elisp(expression) := '{esc}:' $expression {enter};

set print <Linux_printer> = If(Window.Match("xterm|putty"),
			       "setenv PRINTER $1{enter}",
			       Elisp('(setenv "PRINTER" "$1")')
			       Elisp('(setq ps-printer-name "$1")'));

show printer queue = lpq{enter};


## 
## Macros for miscellaneous commands:
## 

  # start up bc with appropriate scale:
run [UNIX] calculator = "bc{enter}scale = 3{enter 2}";

cat dev null	 = "cat > /dev/null" {enter};

invoke startup	 = startup IfHome(-home, -work) {enter};

run unison	 = "do_unison"{enter};

set machine date = "sudo date " Date.Now("%m%d%H%M%Y.%S") {enter};

  # serve current directory via WebBrick at <localhost>:9090
serve here	 = "ruby -run -e httpd . -p 9090"{enter};



### 
### Reusing commands, controlling shell itself:
### 

IfEmacs(Emacs, nonEmacs) := If(Window.Match(emacs), $Emacs, $nonEmacs);
Mark() := {ctrl+space};


## 
## Commands for repeating previous shell commands, possibly with edits:
## 

(short=10|medium=30) history = "history | tail -n $1"{enter};
long                 history = history{enter};

again 0..9 [0..9 [0..9 [0..9]]] = !$1$2$3$4 {enter};
again <letter> [<letter>]	= !$1$2{enter};  # <<<>>>


Recall(number) := IfEmacs("!$number:p"{enter} Wait(500) {up_2}{home}
                             Mark() {end}{esc}w {esc}> {ctrl+y},
                          "!$number"{tab});

recall 0..9 [0..9 [0..9 [0..9]]] = Recall($1$2$3$4);
recall phrase <_anything>	 = Recall($1);


Previous(count) := IfEmacs(When($count,{ctrl+u}$count) {esc}p,
                           {up When($count,_$count)});

recall previous [1..20] = Previous($1);

change command = Previous(1) "{home}{esc}d";
add    option  = Previous(1) "{home}{esc}f" PLACE(" ", " ");
remove option  = Previous(1) "{home}{esc}f{esc}d{ctrl+f}";



## 
## Commands specific to *shell* buffer:
## 

Do(command) := {esc}x $command {enter};

start      shell = Do(shell);
start Ruby shell = Do(shell) irb{enter};

      shell repeat [2..20] = {esc}> REPEAT($1, {esc}p) {enter};
other shell repeat [2..20] = {ctrl+x}o REPEAT($1, {esc}p) {enter} {ctrl+u}-1{ctrl+x}o;

shell (previous=p|next=n) [2..20] = REPEAT($2, {esc}$1);

shell EOF = {ctrl+c}{ctrl+d};

# {ctrl+c}{ctrl+c} to stop subprocess, {ctrl+c}{ctrl+z} to stop it
