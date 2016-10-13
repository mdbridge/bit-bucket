### 
### Voice commands for dealing with Rmail version 24.2 with commands
### changed to require shift key
###
###   Commands for composing messages are in gnu_composition.vcl
### 

include "DNS.vch";
include "gnu.vch";
include "extended_string.vch";
include "locale_Unix.vch";
include "import.vch";

include "letters.vch";

include "Rmail_folder.vch";
include "personal_folder.vch";
include "email_addresses.vch";
include "optional.vch";



Base() := "~/Rmail";



### 
### RMAIL-buffer specific operations:
### 

ShowRmail() := {ctrl+x}b RMAIL{enter};

  # move from Rmail [summary] buffer to associated Rmail buffer:
MessageBuffer()  := Elisp("(pop-to-buffer rmail-buffer)");


  # switch to RMAIL buffer only:
Rmail buffer = ShowRmail() {ctrl+x}1;

  # Retrieve any new messages for the RMAIL folder:
      get (message|messages) = ShowRmail()             GS {ctrl+x}1 H;
      get (mail)             = ShowRmail()             GS {ctrl+x}1 H;# <<<>>>
force get (message|messages) = ShowRmail() Shell(poll) GS {ctrl+x}1 H;

  # grab ownership of folder from other Emacs's then get messages:
grab mail token              = G K EraseToStart() yes {enter} S;



### 
### Operations on current entire folder:
### 

Rmail mode = Do(rmail-mode);  # starting Rmail on an mbox file...


save (message|messages) = S;
expunge messages        = X;

delete spam messages    = {esc}{ctrl+t} \*\*SPAM\*\* {enter} {D_80};


## 
## Summarizing/filtering messages in a folder:
## 

Summarize() := H;

summarize [messages] = Summarize();


filter by <filter>           = $1 {esc}>;
filter by <application>      = {ctrl+u}{esc}{ctrl+t} 
                               "content-type: application/$1" {enter};

filter by phrase <_anything> = {ctrl+u}{esc}{ctrl+t} $1{enter} {esc}>;

filter by [recipient] work <work_email>	    = {esc}{ctrl+r} $1{enter} {esc}>;
filter by  recipient       <personal_email> = {esc}{ctrl+r} $1{enter} {esc}>;
filter by  sender     work <work_email>	    = {esc}{ctrl+f} $1{enter} {esc}>;
filter by  sender          <personal_email> = {esc}{ctrl+f} $1{enter} {esc}>;

<filter> := ( 
	         # keep messages containing the regular expression in header
	      header    =         {esc}{ctrl+s}
	         # keep messages containing the regular expression *anywhere*
	    | keyword   = {ctrl+u}{esc}{ctrl+t}
	    | label     =         {esc}{ctrl+l}
                 # keep messages to/from recipient
	    | recipient =         {esc}{ctrl+r}
	         # one or more comma separated regular expressions:
	    | sender    =       {esc}{ctrl+f}
	    | I sent    =       {esc}{ctrl+f}
	                    "mark.lillibridge,mdl@alum.mit.edu" {enter}
	    | stars     =         {esc}{ctrl+l}*{enter}
	         # keep messages with subjects containing the regular expression
	    | subject   =         {esc}{ctrl+t}
	    );

<application> := ( Excel      = .*excel
	         | PowerPoint = '\(ppt\|.*powerpoint\)'
		 | PDF	      = '\(pdf\|octet-stream[^"]*"[^."]*.pdf"\)'
		 | Word       = msword
		 );


## 
## Sorting messages: 
## 

sort mail by <key> = {ctrl+c}{ctrl+s}$1;

<key> := ( author    = {ctrl+a} | correspondent = {ctrl+c} 
	 | date      = {ctrl+d} | lines         = {ctrl+l}
         | recipient = {ctrl+r} | subject       = {ctrl+s} 
         | sender    = {ctrl+a} );


## 
## Searching for messages in current folder:
## 

  #  search for the next message that has a prompted-for string:
search for message = {esc}s;
  #  repeat search for next message with same search string:
search message     = {esc}s {enter};

  # <<<>>> need way to narrow rmail-buffer afterwards to right message
  # asked for this as a wish (# 13306)
  # as of version 24, this works from a grep buffer as long as the
  # Rmail is already open in Rmail mode
wide search        = MessageBuffer() {ctrl+c}{ctrl+w}  Do(mdl-occur);



### 
### Simple operations on message(s) in the current folder:
### 

  # go to prompted-for absolute message number
go to message              = Do(mdl-rmail-show-message);

  # go to given absolute message number:
mail 0..9 [0..9 [0..9 [0..9]]] = $1$2$3$4 J;


  # go to nearest message whose message number equals r mod 100 from
  # Rmail or summary buffers:
Message(r) := H LineMod($r);

message <r> [<action> [2..10]] = Message($1) REPEAT($3, $2);

<once>       message           =            $1;
<rep>        message [2..20]   = REPEAT($2, $1);


<once>   := ( first=< | last=> | star=A*{enter} | edit=E | resend={ctrl+u}F );
<rep>    := ( next=N   | previous=P
            | delete=D | delete backwards={ctrl+d}
	    | undelete=U
	    );
<action> := ( delete=D | delete backwards={ctrl+d} );

toggle headers = T;


  #  move to top of message body then lift it to top of screen:
message body        = MessageBuffer() 
               {esc}<  LeapRegex(D, ^$) {down}  {ctrl+u}0{ctrl+l};
  # for R&D engineering seminar messages:
seminar description = MessageBuffer() 
               {esc}<  LeapRegex(D, "^Seminar Description") {ctrl+u}0{ctrl+l};


done editing = {ctrl+c}{ctrl+c};

  #  deal with accidental editing of message:
(fix up message|abort editting) = "{ctrl+c}{ctrl+]}";



### 
### Operations across mail folders:
### 

Line(file) := EraseToStart() $file {enter};


## 
## Searching for messages anywhere:
## 

#MatrixHelp() := ~/bin IfHome("","_6") /mairix-0.22/mairix.1);
#MatrixHelp() := IfHome("mairix","~/bin_6/mairix-0.23/mairix.1");
MatrixHelp() := "mairix";

matrix help   = Do2(man, MatrixHelp()) Wait(1000)
       	        {ctrl+x}b "*Man " MatrixHelp() "*"{enter}
		{ctrl+x}1
		Leap(d, "Search patterns") {ctrl+u}0{ctrl+l} {home};

matrix search = Do(mairix-search);
matrix locate = '{esc}!' "mairix -x ''"{left};

matrix update = Do(mairix-update-database);


## 
## Filing messages:
## 

Query() := ? Empty() NoCaps();


file that                        = O                            Query();
file <Rmail_directory> slash     = O EraseToStart() Base() /$1/ Query();

  # send current email message to its default folder:
[prefix 1..20] file default        = When($1,{ctrl+u}$1) O{enter};

[prefix 1..20] file <Rmail_folder> = When($1,{ctrl+u}$1) O Line(Base() /$2);

file copy <Rmail_folder> =
         Elisp('(let ((rmail-delete-after-output nil)) '
	          '(rmail-output "' Base() '/$1" 1))');

file <Rmail_folder> and <Rmail_folder> =
         Elisp('(let ((rmail-delete-after-output nil)) '
	          '(rmail-output "' Base() '/$1" 1))')
        O Line(Base() /$2);

file from <personal_folder>      = O EraseToStart() Base() /$1 {enter};


## 
## Opening mail folders:
## 

Open()        := Do(rmail-input);
Open0(suffix) := Open() EraseToStart() Base() / $suffix;
Open1(suffix) := Open0($suffix) {enter};


open folder                   = Open()     Query();
open <Rmail_directory> slash  = Open0($1/) Query();

open <Rmail_folder>           = Open1($1);
open <Rmail_folder> summary   = Open1($1)  Summarize();

open from <personal_folder>   = Open1($1);

  # experiment; probably doesn't always work:  <<<>>>
open [from] work <work_email> = 
     Open1(h/ Replace3($1, "@hp.com", "", "@hpe.com", "", "_", ".") );



### 
### Dealing with MIME:
### 

include "URLs.vch";

ExportMIME() := Do(mdl-export-mime-to-l1);

  # munpack current buffer to ~/Tmp/export:
UnpackMessage() := ExportMIME() Shell("ruby ~/mail/unpack_l1.rb");
	       

## 
## Viewing MIME messages:
## 

UnpackAndTranslate(filename) :=
    Clipboard.Set("")
    UnpackMessage()
    Do2(mdl-local-pathname-to-PC-pathname, /home/mdl/Tmp/export$filename)
      # wait for unpack and translation to finish:
    Clipboard.WaitForNew("", 10);

UnpackAndDownload() :=
    Clipboard.Set("")
    UnpackMessage()
    Do2(mdl-local-pathname-to-PC-pathname, /home/mdl/Tmp/export)
      # wait for unpack and translation to finish:
    Clipboard.WaitForNew("", 10)
    SyncRsync("-force -replace-dir", @
                                      Replace2(Clipboard.Get(),
				               "p:\Tmp\export", "foil:",
				               "w:\Tmp\export", "work:")
                                      ~/Tmp/export, ~/scratch);

  # these are emergency backups in case clipboard is not working:
import      export = Rsync("-force -replace-dir", @~/Tmp/export,      ~/scratch);
import work export = Rsync("-force -replace-dir", @work:~/Tmp/export, ~/scratch);


view text =
	UnpackMessage()
	   # lookup (text text part or whole messages if none) plus contents:
	{ctrl+x}b *text* {enter}
	{esc}< {esc}> {ctrl+w}
   	{ctrl+x}i Line(~/Tmp/export/ZZB_text_summary.txt)
	Elisp('(visual-line-mode)')
	Elisp('(setq line-move-visual nil)');


  # munpack current buffer to ~/Tmp/export then view that directory:
#[mime] unpack buffer =
unpack buffer = Beep();
[mime] unpack message =
    #UnpackAndTranslate("")
    #AppBringUp("Export", Clipboard.Get())
    UnpackAndDownload()

    AppBringUp("Export", PCfromPC(~/scratch/export))
    Wait(2000)
    {ctrl+r}{ctrl+g};

 # lookup text html part or whole message if none:
#view HTML = UnpackAndTranslate(/_html.html) Lookup(Clipboard.Get());
view HTML = UnpackAndDownload()
            Lookup(PCfromPC(~/scratch/export/ZZA_html.html));

#view meeting request = UnpackAndTranslate(/_meeting.ics) 
#                       AppBringUp("meeting", Clipboard.Get());
view meeting request = 
    UnpackAndDownload()
    AppBringUp("meeting", PCfromPC(~/scratch/export/_meeting.ics));



## 
## Replying to and forwarding MIME messages:
## 

Reply() := R {esc}> {enter};

GoTo()      := {esc}< Leap(D+, "To: ");
GoSubject() := {esc}< Leap(D+, "Subject:");
GoBody()    := {esc}< {ctrl+s}{ctrl+q}{ctrl+j}{ctrl+q}{ctrl+j}{enter};


reply to text =
	UnpackMessage()
	   # lookup text part or whole message if none:
	Reply()
	{ctrl+c}{ctrl+o}
	{ctrl+c}{ctrl+y} {esc}> {ctrl+w}          # X writes header...
	{ctrl+x}1
	{ctrl+x}i Line(~/Tmp/export/ZZB_text_summary.txt)
	Leap(d, "=======") {down} {ctrl+w}
	{esc}> 
	Do(mdl-wrap-long-lines) # <<<>>>
	Do2(mdl-insert-string,"> ")
	{esc}< LineMod(8);


forward [as] seen = 
	MessageBuffer()
	{esc}< {esc}> {esc}w
	F {ctrl+c}{ctrl+o} GoBody()
	"===== Forwarded message follows ====="{enter} {ctrl+y}
	{esc}> {ctrl+w}
	GoTo();

forward text =
	MessageBuffer()
	UnpackMessage()
	{esc}< GoBody() {esc}w
	F {ctrl+c}{ctrl+o}
	GoBody() 
	"===== Forwarded message (text part only) follows ====="{enter} {ctrl+y}
	{ctrl+x}i Line(~/Tmp/export/ZZB_text_summary.txt)
	{ctrl+x}{ctrl+x} Mark() {esc}> {ctrl+w}
	GoBody() 
	{ctrl+s}{ctrl+q}{ctrl+j}{ctrl+q}{ctrl+j}{enter}
	Leap(d, "=======") {down} {ctrl+w}
	GoTo();

  # add text after the <PRE> part...
forward HTML =
	MessageBuffer()
	UnpackMessage()
	{esc}< GoBody() {esc}w
	F {ctrl+c}{ctrl+o}
	GoBody() 
	"<#part type=text/html disposition=inline raw=t>"{enter}
	"<PRE>"{enter}
	"===== Forwarded message (HTML part only) follows ====="{enter}
	{ctrl+y} {backspace}
	"</PRE>"{enter_2}
	{ctrl+x}i Line(~/Tmp/export/ZZA_html.html)
	{ctrl+x}{ctrl+x} Mark() {esc}> {ctrl+w}
	{enter}  "<#/part>"{enter}
	GoTo();



## 
## Experiments:
## 

  # warning: does not do RFC2047 encoding:
alter subject line = GoSubject() {right} Mark() {ctrl+e}{esc}w
    '{esc}:(rmail-set-header "Subject" nil "")' {left_2};

view converted HTML =
	UnpackMessage()
	Shell("(cd ~/Tmp/export; elinks -eval 'set document.browse.margin_width=0' -localhost -no-references -no-numbering -dump-width 75 -dump ZZA_html.html > _converted_html.txt)")
	#Shell("(cd ~/Tmp/export; w3m -ppc 8 -cols 75 -dump ZZA_html.html > _converted_html.txt)")
	{ctrl+x}b "*converted HTML*" {enter}
	{esc}< {esc}> {ctrl+w}
 	{ctrl+x}i Line(~/Tmp/export/_converted_html.txt)
	  # soften unbreakable spaces:
	{esc}< Do(replace-regexp) {ctrl+q}240{enter}{enter} " "{enter} {esc}<
	{ctrl+u}1 Do(toggle-truncate-lines)
	{ctrl+x}1
	;

view E links HTML =
	UnpackMessage()
	Shell("(cd ~/Tmp/export; elinks -eval 'set document.browse.margin_width=0' -localhost -no-references -no-numbering -dump-width 75 -dump ZZA_html.html > _converted_html.txt)")
	{ctrl+x}b "*converted HTML from elinks*" {enter}
	{esc}< {esc}> {ctrl+w}
 	{ctrl+x}i Line(~/Tmp/export/_converted_html.txt)
	  # soften unbreakable spaces:
	{esc}< Do(replace-regexp) {ctrl+q}240{enter}{enter} " "{enter} {esc}<
	{ctrl+u}1 Do(toggle-truncate-lines)
	;

view W three HTML =
	UnpackMessage()
	Shell("(cd ~/Tmp/export; w3m -ppc 8 -cols 75 -dump ZZA_html.html > _converted_html.txt)")
	{ctrl+x}b "*converted HTML from w3m*" {enter}
	{esc}< {esc}> {ctrl+w}
 	{ctrl+x}i Line(~/Tmp/export/_converted_html.txt)
	  # soften unbreakable spaces:
	{esc}< Do(replace-regexp) {ctrl+q}240{enter}{enter} " "{enter} {esc}<
	{ctrl+u}1 Do(toggle-truncate-lines)
	;


reply to converted HTML =
	UnpackMessage()
	Shell("(cd ~/Tmp/export; elinks -eval 'set document.browse.margin_width=0' -localhost -no-references -no-numbering -dump-width 75 -dump ZZA_html.html > _converted_html.txt)")
	Reply()
	{ctrl+c}{ctrl+o}
	{ctrl+c}{ctrl+y} {esc}> {ctrl+w}          # X writes header...
	{ctrl+x}1
 	{ctrl+x}i Line(~/Tmp/export/_converted_html.txt)
	  # soften unbreakable spaces:
	SaveExcursion({esc}< Do(replace-regexp) {ctrl+q}240{enter}{enter} " "{enter})
	{esc}> 
	Do(mdl-wrap-long-lines) # <<<>>>
	Do2(mdl-insert-string,"> ")
	{esc}< LineMod(8);
