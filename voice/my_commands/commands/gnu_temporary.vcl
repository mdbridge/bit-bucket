### 
### Gnu voice commands not yet assigned to a file
### 

include "gnu.vch";


  #
  # run $command once from the start of each line in ranges of lines
  # [$from, $to] where line $from is *before* line $to where lines are
  # denoted as usual mod 100.
  #
  # side effect: defines a keyboard macro  (avoid via macro kill ring?) <<<>>>
  #
EachLine(command, from, to) :=
	SaveExcursion(
	  LineMod($to)           SavePoint(ScratchRegister2())
          LineMod($from) {down}  SavePoint(ScratchRegister3())
	  {up}
  	  "{ctrl+x}(" $command "{ctrl+x})"
	  RestorePoint(ScratchRegister2())  Mark()
	  RestorePoint(ScratchRegister3())
	  Do(apply-macro-to-region-lines)
	);

#################################################################


## 
## Experiments:
## 


justify individually <r> comma <r>  = 
    EachLine({shift+end} Do(fill-region) {ctrl+x}{ctrl+x} # last for bug
    , $1, $2);


  # edmacro-parse-keys is not autoloaded...
  # these produce Vocola code: <<<>>>
SetMacro(keys) := Elisp('(setq last-kbd-macro (edmacro-parse-keys "' $keys '"))');

try it = SetMacro("ESC f <deletechar>") ;

insert macro into Vocola = 'SetMacro("' Elisp('(insert (format-kbd-macro))') '") ';


insert Unicode clipboard = 
    'Clipboard.SetUTF8(UTF8(' 
    EvalTemplate('repr(%s.decode("UTF-8"))[1:]', Clipboard.GetUTF8())
    '))'
    ;


undo correction = {shift} {ctrl+c}u;   # <<<>>>

  # NOTE: set selection commands to not bring up the correction menu
utter = HeardWord(select, that)
	{shift+left}{shift+right}   # cancel possible pending ^c...
	Exchange();

utter space = HeardWord(select, that)
	{shift+left}{shift+right}   # cancel possible pending ^c...
	Exchange() {space} Exchange();

fix utterance = HeardWord(select, that) {esc} 
                Exchange() {Del} Exchange();


#Other(command) := {ctrl+x}o $command {ctrl+u}-1{ctrl+x}o;
#
#refresh line numbers = Do(linum-mode) Do(linum-mode)
#	Other(Do(linum-mode) Do(linum-mode));	     	       




swap <_anything> with <_anything> = {esc}% $1{enter} $2{enter} .;





## 
## Commands for revising:
## 

explode [sentences] <r> comma <r> =
    LineMod($1) {ctrl+space} LineMod($2) 
    Do(narrow-to-region)
    {esc}< Do(replace-regexp) "\. *"{enter}
               ".{ctrl+q}{ctrl+j}{ctrl+q}{ctrl+j}"{enter}
    {esc}< MarkActive() {esc}> {esc}q
    {esc}< Do(replace-regexp) "^ *{ctrl+q}{ctrl+j}"{enter} {enter}
    Do(widen)
    ;

    
Highlight(regexp) := Do2(highlight-regexp, $regexp);

highlight frozen verbs =
    Highlight('\w*\(tion\>\|\tions\>\|sion\>\|ance\>\|ence\>\|ness\>\|ment\>\)');
highlight frozen verbs two =
    Highlight('\w*\(ing\>\)');

highlight meaningless words =
    Highlight('\w*\(kind of\|actually\|particular\|really\|certain\|various\|virtually\|individual\|basically\|generally\|given\|particularly\)');









highlight modifiers =
    Highlight(',[^,.?;]*\.');




vortex screen = 
    Elisp("(goto-char (window-start))") Mark()
    Elisp("(goto-char (- (window-end) 1))")
    {esc}w
    HeardWord(vortex, load, clipboard);


import URL = Do2(eww, {ctrl+y});



include "letters.vch";

Place(before, after) := $before $after {left_ Len($after) };

<type> := (auto | int | integer=int | constant auto="const auto&"
          | auto ref="auto&" );

<type> range loop [<letter>] = Place("for ($1 " When($2,$2,"v") " : ", ")");
