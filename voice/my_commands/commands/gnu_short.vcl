### 
### Gnuemacs level short commands
### 

include "gnu.vch";
include "letters.vch";
include "optional.vch";
include "unpack.vch";



## 
## Implement global short commands using appropriate Emacs commands:
## 

#
# Moving/selecting/killing by words:
# 

MoveLeftStart()    := {esc}b;                   # previous word start
MoveRightEnd()     := {esc}f;                   # next     word end
MoveRightStart()   := {ctrl+c}s  Empty();       # next     word start

SelectLeftStart()  := {ctrl+shift+left};
SelectRightEnd()   := {ctrl+shift+right};
  # this fails to extend existing selection <<<>>>
SelectRightStart() := "{ctrl+c}s," Empty() "," HighMark();  # <<<>>>


#KillLeftStart()    := {esc}{backspace};
#KillRightEnd()     := {esc}d;
  # use mark & single kill to make undo'able in 1 undo:
KillLeftStart()    := "{esc}b,{ctrl+w}," Mark();  
KillRightEnd()     := "{esc}f,{ctrl+w}," Mark();
KillRightStart()   := "{ctrl+c}s,{ctrl+w}," Mark();  # <<<>>>

  # avoid using {ctrl+u}# for maximum portability:
WordMove(action, count) := Unpack3($action, $count);

# 
# cutting/copying and pasting:
# 

Copy()  := {esc}w;
Cut()   := {ctrl+w};
Paste() := {ctrl+y};

# 
# Changing word capitalization:
# 

LowerWord() := l;
UpperWord() := U;
CapWord()   := C;  

ChangeWord(action, count) := Repeat($count, {esc}$action);  # so works with shell

# 
# Copying/killing to the extreme of a compass direction:
# 

CopyStart() := Mark() {home} {esc}w Exchange();
CopyRest()  := Mark() {end}  {esc}w Exchange();

KillStart() := Mark() {home} {ctrl+w};
KillRest()  := Mark() {end}  {ctrl+w};


include "global_short.vch";



## 
## Short versions of overridden/new compass commands:
## 

# 
# Commands for moving a fixed distance in a compass direction:
# 

ID()     := '[a-z0-9_]|[^a-z0-9_]';
Symbol() := '[^ {ctrl+q}{tab}{ctrl+q}{ctrl+j}]|[ {ctrl+q}{tab}{ctrl+q}{ctrl+j}]';

Present(unit)	:= Split($unit, '|', 0);
Absent(unit)	:= Split($unit, '|', 1);

JumpUnit(unit)  := LeapRegexRaw(D+, Present($unit) '+');
FleeUnit(unit)  := LeapRegexRaw(u+, Present($unit) '\|\`')
                   LeapRegexRaw(u+, Absent($unit)  '\|\`');
StartUnit(unit) := LeapRegexRaw(D+, Absent($unit) '+');


<direction> := (
     # 
     # By fragments:
     # 
       flee  fragment = {ctrl+c}b  Empty()
     | jump  fragment = {ctrl+c}f  Empty()
     | kill  fragment = "{ctrl+c}b,{ctrl+w}," Mark()
     | toast fragment = "{ctrl+c}f,{ctrl+w}," Mark()

     # 
     # By tags (/<[^>]*>/)'s:   <<<>>>
     # 
#     | flee  tag      = Leap(u, <)
#     | jump  tag      = Leap(d+, >)
#     | start tag      = Leap(d, <)
#     | kill  tag      = Leap(u, <)  ",{ctrl+w}," MarkActive()
#     | toast tag      = Leap(d+, >) ",{ctrl+w}," MarkActive()
#     | pull  tag      = Leap(d, <)  ",{ctrl+w}," MarkActive()

     # 
     # By S-exps:   <<<>>>
     # 
#     | East      = {esc}{ctrl+f}  # killing of this is ...k
#     | West      = {esc}{ctrl+b}  
#     | North     = {esc}{ctrl+u}
#     | South     = {esc}{ctrl+d}
     | jump list = {esc}{ctrl+n}
     | flee list = {esc}{ctrl+p}

     # 
     # By identifiers (/[\w_]+/):
     # 
     | flee  ID	    = FleeUnit(ID())
     | jump  ID	    = JumpUnit(ID())
     | start ID	    = StartUnit(ID())
     | kill  ID	    = FleeUnit(ID())  ",{ctrl+w}," MarkActive()
     | toast ID	    = JumpUnit(ID())  ",{ctrl+w}," MarkActive()
     | pull  ID	    = StartUnit(ID()) ",{ctrl+w}," MarkActive()

     # 
     # By symbols (nonwhitespace):
     # 
     | flee  symbol = FleeUnit(Symbol())
     | jump  symbol = JumpUnit(Symbol())
     | start symbol = StartUnit(Symbol())
     | kill  symbol = FleeUnit(Symbol())  ",{ctrl+w}," MarkActive()
     | toast symbol = JumpUnit(Symbol())  ",{ctrl+w}," MarkActive()
     | pull  symbol = StartUnit(Symbol()) ",{ctrl+w}," MarkActive()
);

<direction> [<my1to100>] = Unpack3($1, When($2,$2,1));



##
## Jump to location within nearest line whose line number is equal to N
## mod 100:
##

  # start of line
<row> <r> = LineMod($2);



## 
## Leap commands:
## 

include "leap4_short.vch";


#
# Target: one of a set of prespecified regular expression patterns:
# 

  # a character not a whitespace character:
NWS() := "[^ {ctrl+q}{tab}{ctrl+q}{ctrl+j}]";

<pattern> := ( digit = '[0-9]' | letter = '[a-z]' 
	     | non-ASCII = '[^{ctrl+q}{ctrl+a}-~]' );

<leap> [<count>] <pattern> = LeapRegexRaw4($1, $3, When($2,$2,1), m);
<kill> [<count>] <pattern> = LeapRegexRaw4($1, $3, When($2,$2,1), x);



## 
## Move to a specific word/character on the current screen:
## 

match     0..9     =        $1	Empty();
match     <letter> =        $1	Empty();
match big <letter> = {shift+$1} Empty();



## 
## Multistep cutting/copying and pasting:
## 

marking = HighMark();


fetch (region="" | start=Mark() {home} | rest=Mark() {end}) =
    $1 {esc}w Fetch();

unwind   = FetchPoint();

go mouse = SavePoint(*) Mouse.Click() Wait(200);


<copy> := ( copy="" | fetch=Fetch() );
<unit> := ( ID = ID() | symbol = Symbol() 
          | dashes = '[-a-zA-Z0-9]|[^-a-zA-Z0-9]' );

<copy> word   [1..20] = {esc}f {esc}b
                        HighMark() REPEAT($2, {esc}f) HighCopy() $1;
<copy> <unit> [1..20] = JumpUnit($2) FleeUnit($2)
                        HighMark() REPEAT($3, JumpUnit($2)) HighCopy() $1;

<copy> last word   = {end} {esc}b {esc}f {esc}b
                      HighMark() {esc}f             HighCopy() $1;
<copy> last <unit> = {end} FleeUnit($2) JumpUnit($2) FleeUnit($2)
                      HighMark() JumpUnit($2) HighCopy() $1;


<copy> template = Leap(d, '<') FleeUnit(ID()) HighMark()
       		  LeapRegex(d+, '<\([^<>]\|<[^<>]*>\)*>') 
		  HighCopy() $1;

copy password   = Leap(d, "password") LeapRegex(d, " [^ ]") {right}
     		  HighMark() 
		  LeapRegexRaw(d, "[ {ctrl+q}{ctrl+j}{ctrl+q}{ctrl+i}]")
                  HighCopy();



## 
## Experiments: <<<>>>
## 

OperatorChars() := '~!%^&*=+,.<>/?:|' '-';  # - must be last to avoid creating a character range

  # remove spaces around an operator:
collapse = SaveExcursion(
	     LeapRegex(D+, '['  OperatorChars() ']*') {ctrl+u}0{esc}{space}
             LeapRegex(u+, '[^' OperatorChars() ']')  {ctrl+u}0{esc}{space}
	   );


pull ("."|","|":"|";"|paren="("|close paren=")"|question="?") = 
        Leap4(d, $1, 1,  x);

start underscore = LeapRegex(d, "[^a-zA-Z0-9_]_") {right};

<row> other <r> = {ctrl+x}o LineMod($2);
