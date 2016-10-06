###
### Win32Pad voice commands, part II
###
### Version 0.3: word boundaries are Win32Pad's
###

include win32pad.vch;


##
## Jump to start of absolute line number L (directly up to 99,999):
##

  # go to absolute line number $line:
GotoLine(line) := {ctrl+g} WaitForWindow("Go To") $line{enter};

GotoLine3(thousands, hundreds, ones) :=
    GotoLine(Eval($thousands*1000 + $hundreds*100 + $ones));

<n> := 1..99;

  # prompts user for L if no line number given:
absolute line [<n> thousand] [<n> hundred] [<n>] =
    When($1$2$3, GotoLine3(When($1,$1,0), When($2,$2,0), When($3,$3,0)),
                 {ctrl+g});


##
## Operating on lines:
##

Save()   := SaveCurrentLine();
Return() := GotoLine(Variable.Get(":current-line"));

  # {shift+down} doesn't work correctly near blank lines in Win32Pad
  # so use {shift+right}{shift+end} instead to select a line; {end}
  # reduces the chance of a beep (home or end twice in a row beeps)
Apply(count, op) := {end}{home}
	     	    Repeat(When($count,$count,1), {shift+right}{shift+end}) $op;

  # return number of lines in [$top, $bottom) considered as mod 100 references:
Count(top, bottom) := Minus(CalcLine($bottom), CalcLine($top));


<r> := 0..99;

  # Wait's here are for visual feedback to the user:
<op> := ( highlight = ""
        | copy      = Wait(100) {ctrl+c}{right} Return()
        | destroy   = Wait(100) {ctrl+x}
        | yank      = Wait(100) {ctrl+c}{right} Return() {ctrl+v} );

<op> line     [1..99] = Save()				  Apply($2, $1);
<op> next     [1..99] = Save() {home}{down}		  Apply($2, $1);
<op> previous [1..99] = Save() {home}{up_ When($2,$2,1) } Apply($2, $1);

<op> single <r>     = LineMod($2)			  Apply(1, $1);
  # [start row, end row)
<op> <r> comma <r>  = LineMod($2)			  Apply(Count($2,$3), $1);

<op> <r> backwards  = LineMod($2)          {shift+ctrl+home} $1;
<op> <r> onwards    = LineMod($2)   	   {shift+ctrl+end}  $1;

<op> entire buffer  = Save() {ctrl+home}   {shift+ctrl+end}  $1;


ApplyEach(r1, r2, action) := LineMod($r1) Repeat(Count($r1,$r2),
                                                 {home} $action {down});

# These may not work correctly on word-wrapped lines:
comment <r> comma <r>           = ApplyEach($1, $2, "#")        Return();
indent  <r> comma <r> by 1..20  = ApplyEach($1, $2, {space_$3}) Return();
outdent <r> comma <r> by 1..20  = ApplyEach($1, $2, {del_$3})   Return();
