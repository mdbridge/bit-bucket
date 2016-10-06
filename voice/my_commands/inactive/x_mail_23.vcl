### 
### Voice commands for dealing with Rmail version 23.3.1.
###
###   Commands for composing messages are in gnu_composition.vcl
### 

include "DNS.vch";
include "gnu.vch";
include "extended_string.vch";
include "locale_Unix.vch";

include "letters.vch";


Base() := "~/Rmail";



### 
### RMAIL-buffer specific operations:
### 

ShowRmail() := {ctrl+x}b RMAIL{enter};

  # move from Rmail [summary] buffer to associated Rmail buffer:
MessageBuffer()  := Elisp("(pop-to-buffer rmail-buffer)");


  # switch to RMAIL buffer only:
(R. mail|Rmail) buffer       = ShowRmail() {ctrl+x}1;

  # Retrieve any new messages for the RMAIL folder:
      get (message|messages) = ShowRmail()             gs {ctrl+x}1 h;
force get (message|messages) = ShowRmail() Shell(poll) gs;

  # grab ownership of folder from other Emacs's then get messages:
grab mail token              = g k EraseToStart() yes {enter} s;



### 
### Operations on current entire folder:
### 

Rmail mode = Do(rmail-mode);  # starting Rmail on an mbox file...


save (message|messages) = s;
expunge messages        = x;

delete spam messages    = {esc}{ctrl+t} \*\*SPAM\*\* {enter} {d_80};


## 
## Summarizing/filtering messages in a folder:
## 

Summarize() := h;

summarize messages = Summarize();


filter by <filter>           = $1 {esc}>;

filter by phrase <_anything> = {ctrl+u}{esc}{ctrl+t} $1{enter} {esc}>;

filter by <application>      = {ctrl+u}{esc}{ctrl+t} 
                               "content-type: application/$1" {enter};

<filter> := ( 
	         # keep messages containing the regular expression in header
	      header    =         {esc}{ctrl+s}
	         # keep messages containing the regular expression *anywhere*
	    | keyword   = {ctrl+u}{esc}{ctrl+t}
	    | label     =         {esc}{ctrl+l}
                 # keep messages to/from recipient
	    | recipient =         {esc}{ctrl+r}
	         # one or more comma separated regular expressions:
	    | senders   =       h {esc}{ctrl+f}
	    | I sent    =       h {esc}{ctrl+f} 
	                    "mark.lillibridge,mdl@alum.mit.edu" {enter}
	         # keep messages with subjects containing the regular expression
	    | stars     =         {esc}{ctrl+l}*{enter}
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
         | recipient = {ctrl+r} | subject       = {ctrl+s} );


## 
## Searching for messages in current folder:
## 

  #  search for the next message that has a prompted-for string:
search for message = {esc}s;
  #  repeat search for next message with same search string:
search message     = {esc}s {enter};

  # <<<>>> need way to narrow rmail-buffer afterwards to right message
wide search        = MessageBuffer() {ctrl+c}{ctrl+w}  Do(mdl-occur);



### 
### Simple operations on message(s) in the current folder:
### 

  # go to prompted-for absolute message number
go to message              = Do(mdl-rmail-show-message);

  # go to given absolute message number:
mail 0..9                  = $1 j;
mail 0..9 0..9             = $1$2 j;
mail 0..9 0..9 0..9        = $1$2$3 j;
mail 0..9 0..9 0..9 0..9   = $1$2$3$4 j;


  # go to nearest message whose message number equals r mod 100 from
  # Rmail or summary buffers:
Message(r) := h LineMod($r);

message <r>                = Message($1);
message <r> <action>       = Message($1)            $2;
message <r> <action> 2..10 = Message($1) Repeat($3, $2);

<once>       message       =            $1;

<rep>        message       =            $1;
<rep>  2..20 messages      = Repeat($2, $1);
<rep>        message 2..20 = Repeat($2, $1);


<once>   := ( first=< | last=> | star=a*{enter} | edit=e );
<rep>    := ( next=n   | previous=p
            | delete=d | delete backwards={ctrl+d}
	    );
<action> := ( delete=d | delete backwards={ctrl+d} );


  #  move to top of message body then lift it to top of screen:
message body = MessageBuffer() 
               {esc}<  LeapRegex(D, ^$) {down}  {ctrl+u}0{ctrl+l};


done editing = {ctrl+c}{ctrl+c};

  #  deal with accidental editing of message:
(fix up message|abort editting) = "{ctrl+c}{ctrl+]}";



### 
### Operations across mail folders:
### 

include "Rmail_folder.vch";
include "personal_folder.vch";
include "email_addresses.vch";

Line(file) := EraseToStart() $file {enter};


## 
## Searching for messages anywhere:
## 

matrix help   = Do2(man, ~/bin/mairix-0.22/mairix.1) Wait(1000)
       	        {ctrl+x}b "*Man ~/bin/mairix-0.22/mairix.1*"{enter}
		{ctrl+x}1
		Leap(d, "Search patterns") {ctrl+u}0{ctrl+l} {home};

matrix search = Do(mairix-search);
matrix locate = '{esc}!' "~/bin/mairix -x ";

matrix update = Do(mairix-update-database);


## 
## Filing messages:
## 

Query() := ? Empty() NoCaps();


file that                        = o                            Query();
file <Rmail_directory> slash     = o EraseToStart() Base() /$1/ Query();

  # send current email message to its default folder:
             file default        =            o{enter};
prefix 1..20 file default        = {ctrl+u}$1 o{enter};

             file <Rmail_folder> =            o Line(Base() /$1);
prefix 1..20 file <Rmail_folder> = {ctrl+u}$1 o Line(Base() /$2);

file <Rmail_folder> and <Rmail_folder> =
         Elisp('(let ((rmail-delete-after-output nil)) '
	           '(rmail-output "' Base() '/$1" 1))')
        o Line(Base() /$2);

file from <personal_folder>      = o EraseToStart() Base() /$1 {enter};


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
     Open1(h/ Replace2($1, "@hp.com", "", "_", ".") );



### 
### Dealing with MIME:
### 

include "URLs.vch";

ExportMIME() := Do(mdl-export-mime-to-l1);

  # munpack current buffer to ~/Tmp/export:
UnpackBuffer() := ExportMIME() Shell("ruby ~/mail/unpack_l1.rb");
	       

## 
## Viewing MIME messages:
## 

view text =
	UnpackBuffer()
	   # lookup (text text part or whole messages if none) plus contents:
	{ctrl+x}b *text* {enter}
	{esc}< {esc}> {ctrl+w}
	"{esc}:(longlines-mode 0){enter}"
   	   {ctrl+x}i Line(~/Tmp/export/_text.txt)
	"{esc}:(longlines-mode 1){enter}";


  # munpack current buffer to ~/Tmp/export then view that directory:
[mime] unpack buffer =
	Do2(mdl-local-pathname-to-PC-pathname, /home/mdl/Tmp/export/l1)
	UnpackBuffer()
	Wait(2000)
	AppBringUp("Export", "explorer /select," Clipboard.Get())
	Wait(2000)
	{ctrl+r}{ctrl+g};

view HTML =
	Do2(mdl-local-pathname-to-PC-pathname, /home/mdl/Tmp/export/_html.html)
	UnpackBuffer()
	   # lookup text html part or whole message if none:
	Lookup(Clipboard.Get());

view meeting request =
	Do2(mdl-local-pathname-to-PC-pathname, /home/mdl/Tmp/export/_meeting.ics)
	UnpackBuffer() Wait(1000)
	AppBringUp("meeting", Clipboard.Get());


## 
## Replying to and forwarding MIME messages:
## 

GoTo()      := {esc}< Leap(D+, "To: ");
GoSubject() := {esc}< Leap(D+, "Subject:");
GoBody()    := {esc}< {ctrl+space} {ctrl+s}{ctrl+q}{ctrl+j}{ctrl+q}{ctrl+j}{enter};


reply to text =
	UnpackBuffer()
	   # lookup text part or whole message if none:
	r{enter}
	{ctrl+c}{ctrl+o}
	{ctrl+c}{ctrl+y} {esc}> {ctrl+w}          # X writes header...
	{ctrl+x}1
	{ctrl+x}i Line(~/Tmp/export/_text.txt)
	Leap(d, "=======") {down} {ctrl+w}
	{ctrl+space} {esc}> Do2(mdl-insert-string,"> ")
	{esc}< LineMod(8);


forward [as] seen = 
	MessageBuffer()
	{esc}< {esc}> {esc}w
	f {ctrl+c}{ctrl+o} GoBody()
	"===== Forwarded message follows ====="{enter} {ctrl+y}
	{esc}> {ctrl+w}
	GoTo();

forward text =
	MessageBuffer()
	UnpackBuffer()
	{esc}< GoBody() {esc}w
	f {ctrl+c}{ctrl+o}
	GoBody() 
	"===== Forwarded message (text part only) follows ====="{enter} {ctrl+y}
	{ctrl+x}i Line(~/Tmp/export/_text.txt)
	{ctrl+x}{ctrl+x} {ctrl+space} {esc}> {ctrl+w}
	GoBody() 
	{ctrl+space} {ctrl+s}{ctrl+q}{ctrl+j}{ctrl+q}{ctrl+j}{enter}
	Leap(d, "=======") {down} {ctrl+w}
	GoTo();

  # add text after the <PRE> part...
forward HTML =
	MessageBuffer()
	UnpackBuffer()
	{esc}< GoBody() {esc}w
	f {ctrl+c}{ctrl+o}
	GoBody() 
	"<#part type=text/html disposition=inline raw=t>"{enter}
	"<PRE>"{enter}
	"===== Forwarded message (HTML part only) follows ====="{enter}
	{ctrl+y} {backspace}
	"</PRE>"{enter_2}
	{ctrl+x}i Line(~/Tmp/export/_html.html)
	{ctrl+x}{ctrl+x} {ctrl+space} {esc}> {ctrl+w}
	{enter}  "<#/part>"{enter}
	GoTo();



## 
## Experiments:
## 

alter subject line = GoSubject() {right} {ctrl+space}{ctrl+e}{esc}w
    '{esc}:(rmail-set-header "Subject" nil "")' {left_2};
