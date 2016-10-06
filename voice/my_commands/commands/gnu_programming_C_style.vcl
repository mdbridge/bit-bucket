### 
### Voice commands for programming in languages with C-style syntax
### 
###   Commands for commenting are in gnu_programming.vcl
### 

include "gnu.vch";


### 
### Minimalist statement templates:
### 

Place(before, after) := $before $after {left_ Len($after) };
BackTil(target)      := Leap(u, $target);
MonoSpace()          := {esc}{space};
BlockPair()          := MonoSpace() "{{}" {enter_2} "}" {tab}{up}{tab};
EndBlockPair()       := {end} BlockPair();


## 
## Stand-alone statement (head)s, excluding loops
## 
## Add a body once done dictating a head via "EndNBlock pair" or "EndNFeed".
## 

(   do           = "do" EndBlockPair() # finish with "climb while statement"
  | do while     = "do" EndBlockPair() {down}{end} Place(" while(",");")
  | if           = Place("if (",")")
  | switch       = Place("switch (",")")
  | synchronized = Place("synchronized (",")")
  | try          = "try" EndBlockPair()

  | class        = "class"  EndBlockPair() {down}{end} ";" 
                                           {up_2}{end} {left_2} " "
  | struct       = "struct" EndBlockPair() {down}{end} ";" 
                                           {up_2}{end} {left_2} " "
  | namespace    = "namespace" EndBlockPair() {up}{end} {left_2} " "

  | C error      = Place("cerr << "," << std::endl;")
  | C out        = Place("cout << "," << std::endl;")
) statement = {tab} $1 Empty();


## 
## Statement (clauses) that can/must follow '}'
## 
##     To modify current line, omit climb.  To first move to first
## line containing '}', add climb.  Omitting climb in the must follow
## cases attempts to Do What I Mean (DWIM) by climbing from the start
## of the current line.
## 

Climb() := Leap(D, '}') {end};
<climb> := (climb [brace] = Climb());

  # can follow:
[<climb>] ( else    = "else" {enter}{tab}
          | else if = Place("else if (",")")
          | while   = Place("while (",")")
          ) statement = $1 {end} MonoSpace() {tab} $2 Empty();
[<climb>] else block  = $1 {end} MonoSpace() {tab} "else" EndBlockPair() Empty();

  # must follow:
[<climb>] ( catch          = Place("catch (", " e)")
          | wildcard catch = "catch (...)" EndBlockPair()
          | finally        = "finally" EndBlockPair()
          ) statement = When($1,$1,{home} Climb()) {tab}{end} 
	    	      	MonoSpace() $2 Empty();

## 
## Loops:
## 

loop forever = {tab} "for (;;)" EndBlockPair();

# Java iterators:
loop on iterator = {enter}{tab}  "while (iterator.hasNext())" EndBlockPair()
                   {up_2}{end}   Place("Iterator iterator = ",".iterator();");


# 
# Simple integral for loops; e.g.,
#
#   for (int i=0; i<???; i++)
#   for (int j=0,jend=???; j<jend; ++j)    [optimized variant]
#
# where point is left at ??? for the user to dictate the limiting
# value.  The loop type (if any) and variable name are specified by
# the user either via voice command arguments or an elisp prompt.
#

<index_variable> := ( i | j | k | India=i | Juliet=j | Kilo=k | Romeo=r | Charlie=c);
<index_type>     := ( untyped = "" | index = "int " | int = "int " 
                    | integer = "int " );

loop on <index_type> <index_variable> =
	{tab} Place("for ($1$2=0; $2<","; $2++)");

  # similar, but prompts user for optional type and required variable name:
loop on index = {tab} Do(mdl-loop-on-index);

optimized loop on <index_type> <index_variable> =
        {tab} Place("for ($1$2=0,$2end=","; $2<$2end; ++$2)");


## 
## Commands for finishing or continuing statements:
## 

EndNFeed  = {end} {ctrl+j};
EndNBrace = {end} MonoSpace() {;
EndNParen = {end} ")";
EndNSemi  = {end} ";";

SemiLine  = ";"   {ctrl+j};
SemiNFeed = ";"   {ctrl+j};

    block pair =    BlockPair();
EndNBlock pair = EndBlockPair();

climb brace    = Climb();
jump  brace    = Climb() {enter}{tab};


# Wrap next N lines with braces, with 1st brace on end of current line:
wrap with braces 1..20 =
	{end} MonoSpace() {
	{down}{home} Mark() {down_$1} {esc}{ctrl+\}
	{ctrl+o} } {tab};

# same but wraps lines from next line until just before given line mod 100:
wrap with braces until <r> =
	{end} MonoSpace() {
	{down}{home} Mark() LineMod($1) {esc}{ctrl+\}
	{ctrl+o} } {tab};



### 
### Aligning programming constructs:
### 

  # like tab in text mode in any mode:
align tab       =            Do(indent-relative);
align tab 1..20 = Repeat($1, Do(indent-relative));


Align(regexp) := Do2(align-regexp, $regexp);

  # only align exp's where preceeded by at least 1 space:
LooseAlign(exp) := 
	{ctrl+u} Do(align-regexp) EraseToStart() '\(\s-+\)' $exp {enter}
	{enter_2} n;

ComplexAlign(before, after, spacing) :=
    {ctrl+u} Do(align-regexp) $after{home}$before{enter}  {enter}
		              EraseToStart() $spacing{enter} n;

<target>:= (
      double slash	= ComplexAlign("", "//", 2)
    | after colon       = ComplexAlign(":", "[^ ]", 1)

    | parameter		= Align("[^ ]+" "[,)]")

    | variable		= Align("[^ ]+" " *[=;]")
      # attempt to handle many constructors containing spaces as well;
      # no nested ()'s yet:
    | complex variable	= Align("[a-zA-Z0-9_]+" 
      	      		        "\(?:([^)]*)\)?" 
      	      		        "\(?:\[[^]]*\]\)?" 
				" *[=;]")
      # variable then "=":
    | assignment        = Align("[^ ]+" " *[=;]")
                          LooseAlign("=")

    |       function	= ComplexAlign("^ *\(?:[a-zA-Z0-9_].*?\)?", 
    	    		  	       "[~a-zA-Z0-9_]+ *(", 1)
    | tight function	= ComplexAlign("^ *\(?:[a-zA-Z0-9_].*?\)?", 
                                       "[~a-zA-Z0-9_]+(", 1)

    |        identifier	= Align("[a-zA-Z0-9_]")
    | second identifier = ComplexAlign("[A-Za-z0-9_]+" "[^A-Za-z0-9_]*?",
      	     		  	       "[A-Za-z0-9_]", 1)
#    | third  identifier = Repeat(3, StartIdentifier())
#    | last   identifier = {end} FleeIdentifier()

    | D oxygen		=   # align variable names after @param's:
                          ComplexAlign("^ *\* *@param[^ ]+", "[^ ]", 1)
			    # align text afterwards, including non-param lines:
      			  ComplexAlign("^ *\*"
                              "\(?: *@param[^ ]+ *[a-zA-Z0-9_]+\)?", 
                              "[^ ]", 3)

       # separate versions:
    | D oxygen text     = ComplexAlign("^ *\*"
                              "\(?: *@param[^ ]+ *[a-zA-Z0-9_]+\)?", 
                              "[^ ]", 3)
    | D oxygen name	= ComplexAlign("^ *\* *@param[^ ]+", "[^ ]", 1)


       # multiple groups of XXX,'s:
    | commas            = {ctrl+u} Do(align-regexp)
      			  EraseToStart() '\(\s-*[^, ]*\),'{enter}
			  EraseToStart() '-1'{enter}
			  {enter} y
);

align <target> <r> comma <r> = LineMod($2) Mark() LineMod($3) $1;
