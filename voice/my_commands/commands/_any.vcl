### 
### Voice commands that should be usable with any application.
### 
###   Distinguished from commands that can be issued from anywhere,
###   but don't actually interact with the current application.  
###   E.g., "lookup Java".
### 
###   May be overridden for particular applications.
### 

include "DNS.vch";


## 
## Special non-compass keys with names:
## 

  # escape is a short command...
triple escape        = {esc_3};

(right|context) menu = {shift+f10};

abort                = {ctrl+g};          # abort in Gnu emacs...
line feed            = {ctrl+j};



## 
## Misc. global voice commands
## 

force dictate <_anything> = SendSystemKeys($1);

please find                    = {ctrl+f} NoCaps();
please find clipboard          = {ctrl+f} {ctrl+v};
please find phrase <_anything> = {ctrl+f} $1;

  # saves to clipboard; captures:
snapshot (window=alt|screen=ctrl) = {$1+Prtsc};



include "printables.vch";

# 
# type Windows clipboard; do not use for long text, or text containing
# control characters, etc.
#
paste keys = PrintablesToKeys(Replace(Clipboard.Get(), Eval("chr(13)"), ""));



##
## Editing combos
##

# a bunch of programming-related versions are in gnu_programming_C_style.vcl

  # these don't seem very useful anymore given last key <prn>...  <<<>>>
#EndNSpace = {end} {shift+space};
#EndNStop  = {end} ".  ";
#EndNComma = {end} ", ";
#EndNColon = {end} ":";



## 
## Filling in forms:
## 

include "fields.vch";

type  <field> = $1;

<tab> := (tab={tab});

<field> tab [<field> [<tab> [<field> [<tab>]]]] = $1{tab}$2$3$4$5;



## 
## Typing machine names:
## 

include "machines.vch";

machine <machine> = Machine($1);



## 
## Typing directories:
## 

# specific shells have type directory ... commands with appropriate quoting

include "directories.vch";
include "file_access.vch";

type Unix directory       <UNIX> [/ <COM>] =              $1 When($2, /$2);

type PC   directory [<D>] <UNIX> [/ <COM>] = UNIXfromPC($1$2 When($3,/$3));
type PC   directory       <PC>   [/ <COM>] = PCfromPC  (  $1 When($2,/$2));



## 
## Typing lists of email addresses:
## 

include "email_addresses.vch";

<comma> := (comma = ", ");

[<comma>] email for work <work_email>     = $1$2;
[<comma>] email for      <personal_email> = $1$2;



## 
## Mailing clipboard/selection to someone:
## 

include "switch.vch";

<clipboard> := ( clipboard="" | selection={ctrl+c} Wait(100) );

<context_mail> <clipboard> to      <personal_email> = $2 MailClipboard($1,$3);
<context_mail> <clipboard> to work <work_email>     = $2 MailClipboard($1,$3);



## 
## Signatures:
## 

thanks bang = Cap() HeardWord(thanks) "!  " Cap();

sign me     = "- Mark{enter}";
sign us     = "- Mark & Laura{enter}";
sign thanks = "- Thanks," {enter}
              "  Mark"    {enter};
sign sorry  = "- Sorry,"  {enter}
              "  Mark"    {enter};

  # Emacs only:
sign end    = {ctrl+end} {enter_3} "- Mark"{enter} 
              {up_2} {ctrl+x}{ctrl+o} {down_2};  # ensure only 1 blank line

sign next   = {end} {enter_3} "- Mark"{enter}
              {up_2} {ctrl+x}{ctrl+o} {down_2};  # ensure only 1 blank line



## 
## A command for producing sequentially increasing numbers:
## 

include "numbers.vch";

  # what to do for each number:
<op> := ( comma       = Variable.Get(_i) ","
        | comma space = Variable.Get(_i) ", "
        | space       = Variable.Get(_i) " "
        | list        = Variable.Get(_i) ". "{enter}

        | right       = Variable.Get(_i) {right}
        | down        = Variable.Get(_i) {down}
        | slap        = Variable.Get(_i) {enter}

	| at end      = {end} Variable.Get(_i) {down}{end}
        | inserting   = Variable.Get(_i) {left_ Len(Variable.Get(_i))} {down}
        | overwriting = Variable.Get(_i) {Del_  Len(Variable.Get(_i))}
                                         {left_ Len(Variable.Get(_i))} {down}
	);

number <my0to99> to <my0to99> <op> =
       Variable.Set(_i, $1)
       Repeat(Eval($2-$1+1),
	   $3 Variable.Set(_i, Eval(Variable.Get(_i) +1))
       );

  # example: number backwards 10 to 1 list
number backwards <my0to99> to <my0to99> <op> =
       Variable.Set(_i, $1)
       Repeat(Eval($1-$2+1),
	   $3 Variable.Set(_i, Eval(Variable.Get(_i) -1))
       );



## 
## Voice commands for creating/editing twiki pages
## 

star 0..19       = {space_$1}*{space};



## 
## Forming symbols from English words:
## 
##   See also gnu_programming.vcl
## 

T(anything) := $anything;

Score(x) := Replace($x, " ", "_");

UpperCase(x) := Replace(Upper($x), " ", "");

  # does not handle all cap-words correctly:
#CamelCase(x) := Mid(Replace(Capitalize("x" $x), " ", ""),1);

UpperCamelCase(x) := 
    EvalTemplate('"".join([x[0].capitalize()+x[1:] for x in %s.split()])', $x);

CamelCase(x) := Mid(UpperCamelCase("x" $x),1);



Studley	 <_anything> = UpperCamelCase(T($1));  # WaterFire
wimpy	 <_anything> =      CamelCase(T($1));  # waterFire

    bend <_anything> = <      Score(T($1)) >;  # <water_fire>
web bend <_anything> = "&lt;" Score(T($1)) >;  # &lt;water_fire>

slam	 <_anything> = Replace(T($1)," ","");  # waterfire
yell	 <_anything> = UpperCase(T($1));       # WATERFIRE

snake	 <_anything> = Score(T($1));           # water_fire
spine	 <_anything> = Replace(T($1)," ","-"); # water-fire



pre Stud <_anything> = UpperCamelCase(T($1)) '::';  # WaterFire::
