### 
### Win32pad voice commands, part II
### 
### Version 0.1: line numbers limited to 0..99, word boundaries are win32pad's
### 

## 
## Operating on lines:
## 

<r> := 0..99;

LineMod(n) := {ctrl+g}$n{enter};

  # Wait's here are for visual feedback to the user:
<op> := ( highlight = "" | copy = Wait(100) {ctrl+c}{right} 
        | destroy   = Wait(100) {ctrl+x} );

  # {shift+down} doesn't work correctly near blank lines in win32pad
  # so use {shift+right}{shift+end} instead to select a line:
Apply(count, op) := {home} Repeat($count, {shift+right}{shift+end}) $op;


<op> line           =               Apply(1,  $1);
<op> line 1..99     =               Apply($2, $1);

<op> next           = {home}{down}  Apply(1,  $1);
<op> next 1..99     = {home}{down}  Apply($2, $1);

<op> previous       = {home}{up}    Apply(1,  $1);
<op> previous 1..99 = {home}{up_$2} Apply($2, $1);

<op> single <r>     = LineMod($2)   Apply(1, $1);
  # [start row, end row)
<op> <r> comma <r>  = LineMod($2)   Apply(Eval($3 - $2), $1);

<op> <r> backwards  = LineMod($2)   {shift+ctrl+home} $1;
<op> <r> onwards    = LineMod($2)   {shift+ctrl+end}  $1;

<op> entire buffer  = {ctrl+home}   {shift+ctrl+end}  $1;


ApplyEach(r1, r2, action) := LineMod($r1) Repeat(Eval($r2-$r1), 
                                                 {home} $action {down});

# These may not work correctly on word wrapped lines:
comment <r> comma <r>           = ApplyEach($1, $2, "#");
indent  <r> comma <r> by 1..20  = ApplyEach($1, $2, {space_$3});
outdent <r> comma <r> by 1..20  = ApplyEach($1, $2, {del_$3});
