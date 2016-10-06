### 
### Voice commands for programming in languages with C-style syntax:
### 
###   Commands for commenting are in gnu_programming.vcl
### 

include "gnu.vch";


### 
### Minimalist statement templates:
### 

BackTil(target) := Leap(u, $target);


##
## C-style statements:
##
##   The leading elastic spaces are for cases like repeat^ while, else^ if.
##

  # KEYWORD (^):
<paren_argument> := ( if | "else if" | switch | synchronized | while );

<paren_argument> statement = {tab} {shift+space} "$1 ()" {left} Empty();
else             statement = {tab} {shift+space} else {enter}{tab};


  # {\n^\n}$afterwards
BlockPair(afterwards) := {shift+space} {{} {enter_2} } $afterwards
		         {tab}{up}{tab};

  # KEYWORD {\n^\n}:
<block_argument> := ( else block = else | try statement = try );

<block_argument>           = {tab} {shift+space} $1 BlockPair("");

do-while         statement = {tab}               do BlockPair(" while ();");


  # move after next } unless we are already just past one (0-1 characters):
BlockEnd() := {left_2} Leap(D+, '}');

catch            statement = BlockEnd() {shift+space} "catch ( e)"
                             BlockPair("") BackTil(" e");
wildcard catch   statement = BlockEnd() {shift+space} "catch (...)"
                             BlockPair("");
finally          statement = BlockEnd() {shift+space} finally BlockPair("");


## 
## C++ statements:
## 

declare class = {tab} "class  " BlockPair(";") BackTil("class ") {right_6};


##
## Java/C style loop statements:
##

loop forever = {tab} "for (;;)" BlockPair("");


#
#   user dictates maximum index+1  (+0 if use "press equal" first)
#   optionally, the user then says "set start" then dictates the
#   loop's starting value
#
<index_variable> := ( i | j | k | India=i | Juliet=j | Kilo=k );
<index_type>     := ( untyped = "" | index = "int " | int = "int " 
                    | integer = "int " );

loop on <index_type> <index_variable> =
           {tab} "for ($1$2=0; $2<; $2++)" BackTil(";");

  # similar, but prompts user for optional type and required variable name:
loop on index                         = {tab} Do(mdl-loop-on-index);

optimized loop on <index_type> <index_variable> =
           {tab} "for ($1$2=0,$2end=; $2<$2end; ++$2)" BackTil("=") {right};


  #  used to replace the starting value of an index Loop:
set start = BackTil("=0") {right_2}{backspace};


# 
# Java iterators:
# 
loop on iterator =
	"Iterator iterator = .iterator();" {tab}{enter}
	"while (iterator.hasNext())" {tab} BlockPair("") BackTil(".i");


## 
## Commands for finishing or continuing statements:
## 

EndNFeed  = {end} {ctrl+j};
EndNBrace = {end} {shift+space}{;
EndNParen = {end} ")";
EndNSemi  = {end} ";";

SemiLine  = ";"   {ctrl+j};
SemiNFeed = ";"   {ctrl+j};


    block pair =       BlockPair("");
EndNBlock pair = {end} BlockPair("");


climb brace    = Leap(D, '}') {ctrl+e};
climb while    = Leap(D, '}') Leap(D, '(');

jump  brace    = Leap(D, '}') {ctrl+e}{enter}{tab};



wrap with braces 1..20 =
	{tab}{shift+space} {{} {down}{ctrl+a}
	Mark() {ctrl+u}$1{down} {esc}{ctrl+\}
	{ctrl+o} } {tab};


## 
## Aligning programming constructs:  <<<>>>
## 

  # like tab in text mode in any mode:
align tab       =            Do(indent-relative);
align tab 1..20 = Repeat($1, Do(indent-relative));


#   each of these requires from < to

  #
  # run $command once from the start of each line in ranges of lines
  # [$from, $to] where line $from is *before* line $to where lines are
  # denoted as usual mod 100.
  #
  # side effect: defines a keyboard macro  (avoid via macro kill ring?) <<<>>>
  #
EachLine(command, from, to) :=
	SaveExcursion(
          LineMod($from) 
  	  "{ctrl+x}(" $command "{ctrl+x})"
  	  {down}{home}	Mark()
  	  LineMod($to) 
  	  Do(apply-macro-to-region-lines)
	);


Selected(commands, from, to) := SaveExcursion( LineMod($from) Mark()
		   	                       LineMod($to) $commands );

Align(commands, from, to) := 
        EachLine($commands '|<>|', $from, $to)
	Selected(Do2(steve-align-regexp, '|<>|'), $from, $to)
        EachLine(Leap(d, '|<>|') {Del_4}, $from, $to);


JumpIdentifier()  := LeapRegex(D+, '[a-z0-9_]+');
FleeIdentifier() := LeapRegex(u+, '[a-z0-9_]\|\`') LeapRegex(u+, '[^a-z0-9_]\|\`');
StartIdentifier() := LeapRegex(D+, '[^a-z0-9_]+');

<thing> := (
            function = LeapRegex(d, "[~a-z0-9_]+[ ]*(")
    | tight function = LeapRegex(d, "[~a-z0-9_]+(")

    | variable       = LeapRegex(d, "\(\[[^][]*\] *\)?" # optional [...] 
			            "[=;]")
                       #LeapRegex(u+, '[a-z]\|\`') # skip any #s, as in [10]...
		       FleeIdentifier()
    | argument       = LeapRegex(d, "\(\[[^][]*\] *\)?" # optional [...] 
                                    "\([,)]\|$\)")
		       FleeIdentifier()

    | second identifier = Repeat(2, StartIdentifier())
    | third  identifier = Repeat(3, StartIdentifier())
    | last   identifier = {end} FleeIdentifier()
);

      align <thing> <r> comma <r> = Align($1, $2, $3);
  # remove extra space before alignment point:
tight align <thing> <r> comma <r> = Align($1 {esc}{space}, $2, $3);

test  align <thing> <r> comma <r> = EachLine($1 '|<>|', $2, $3);
