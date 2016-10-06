###
### Dealing with elisp:
###

include "gnu.vch";


## 
## Evaluating/loading elisp code:
## 

  # prompt for elisp expression to evaluate:
eval expression         = Do(eval-expression);
#eval expression again   = Do(eval-expression) {up}{enter};

# see also "evaluate <range>" in gnu_range.vcl

  # line feed in this mode evaluates the expression before the point:
lisp interaction mode   = Do(lisp-interaction-mode);


# 
# Loading particular (test) files:
# 

<file> := ( test | test two = "test2" | fill );

eval <file> = Elisp('(load-file "/home/mdl/elisp/test/$1.el")');



## 
## Running (test) elisp routines:
## 

<routine> := ( mdl | registered=YankFromRegister(a) );

run <routine> = Do($1);



## 
## Byte compiling files:
## 

  # only for foil and ts-rhel7.labs.hpecorp.net:
byte compile files = 
    Do(compile) EraseToStart() "(cd ~/elisp; make compile)" {enter};


<compile> := (compile | recompile);

      <compile> elisp = Do(byte-recompile-directory) 
                        EraseToStart() ~/elisp {enter};
force <compile> elisp = Do(byte-force-recompile)
                        EraseToStart() ~/elisp {enter};


## 
## Templates:
## 

paren call <_anything> = "(" Replace($1, " ", "-");



## 
## Miscellaneous:
## 

outermost   = Repeat(10,{esc}{ctrl+u});

  # experimental:  <<<>>>
indent Lisp = SaveExcursion(Repeat(10, {esc}{ctrl+u}) Wait(100) {esc}{ctrl+q} 
                                       Wait(100));


## 
## Experimental:
## 

Close(movement) := {ctrl+u}10")" {left_10} $movement {esc}{ctrl+f} {ctrl+k};

close line           = Close({home}) {enter}{tab};
close line <my0to99> = Close(LineMod($1)) {enter}{tab};

close all            = Close(Repeat(10,{esc}{ctrl+u}) Wait(100)) {enter};


transpose group = {esc}{ctrl+t};
