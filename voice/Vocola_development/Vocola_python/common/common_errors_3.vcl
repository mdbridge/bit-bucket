### 
### Common syntax errors made when writing Vocola 2 programs, part 3.
### 

<list> := dummy;


## 
## Confusing equals and assignment:
## 

<list> = (Fred | mark);
another = keystrokes and more keystrokes;
resynchronize = here;

<list> = Fred | mark;
another = keystrokes and more keystrokes;
resynchronize = here;


<list> = (Fred = 1 | mark);
another = keystrokes and more keystrokes;
resynchronize = here;

<list> = Fred = 1 | mark;
another = keystrokes and more keystrokes;
resynchronize = here;


bar0() = action;
another = keystrokes and more keystrokes;
resynchronize = here;

  # next UNDETECTABLE (valid code):
bar1(argument) = action;
another = keystrokes and more keystrokes;
resynchronize = here;

bar2(argument, argument) = action;
another = keystrokes and more keystrokes;
resynchronize = here;



## 
## Confusing which directives start with "$":
## 

$include "Fred.vch";
another = keystrokes and more keystrokes;
resynchronize = here;

set variable value;
another = keystrokes and more keystrokes;
resynchronize = here;



## 
## Bad directives:
## 

include environment_ $COMPUTERNAME .vch;
another = keystrokes and more keystrokes;
resynchronize = here;

$set variable;
another = keystrokes and more keystrokes;
resynchronize = here;

$set numbers one, two, three, four;
another = keystrokes and more keystrokes;
resynchronize = here;



## 
## Empty alternatives in context statement:
## 
## These are currently legal.
## 

#| Empty alternative in context:
#another = keystrokes and more keystrokes;
#resynchronize = here;
#
#first | Empty alternative in context|   :
#another = keystrokes and more keystrokes;
#resynchronize = here;
#
#first | Empty alternative in context|   #a comment
#:
#another = keystrokes and more keystrokes;
#resynchronize = here;



## 
## Attempting to make things optional that are not allowed:
## 

only [one word] may be optional = sadly;
resynchronize = here;

no [(red | blue)] = menus;
resynchronize = here;

can [1..99] ranges be = optional;  # Perl compiler has bug and misses this
resynchronize = here;

can [<list>] lists be = optional;  # Perl compiler has bug and misses this
resynchronize = here;

can [<_anything>] dictation be = optional;  # Perl compiler has bug and misses this
resynchronize = here;

nested [[optional]] = forbidden;
resynchronize = here;



## 
## Miscellaneous:
## 

foo := action;
another = keystrokes and more keystrokes;
resynchronize = here;


A command = "function"(argument);
another = keystrokes and more keystrokes;
resynchronize = here;


function(illegal$) := action;
another = keystrokes and more keystrokes;
resynchronize = here;


a command = call[argument];
another = keystrokes and more keystrokes;
resynchronize = here;


Fred's bookshop:
another = keystrokes and more keystrokes;
resynchronize = here;



Crap 2
