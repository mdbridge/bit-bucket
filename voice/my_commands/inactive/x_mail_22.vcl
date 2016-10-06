### 
### Voice commands for dealing with Rmail version 22 and earlier.
###
###   Commands for composing messages are in x_composition.vcl
### 

primary emacs:


include "DNS.vch";
include "gnu.vch";
include "locale_Unix.vch";

include "letters.vch";


Base() := "~/Rmail";  # <<<>>>



### 
### RMAIL-buffer specific operations:
### 

  # move from Rmail [summary] buffer to associated Rmail buffer:
MessageBuffer()  := Elisp("(pop-to-buffer rmail-buffer)");

  # switch to RMAIL buffer only:
(R. mail|Rmail) buffer       = {ctrl+x}b RMAIL{enter} {ctrl+x}1;

  # Retrieve any new messages for the RMAIL folder:
      get (message|messages) = {ctrl+x}b RMAIL{enter}             gs;
force get (message|messages) = {ctrl+x}b RMAIL{enter} Shell(poll) gs;

  # grab ownership of folder from other Emacs's then get messages:
grab mail token              = g YES {enter} s;



### 
### Operations on current entire folder:
### 

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
## Searching for messages:
## 

  #  search for the next message that has a prompted-for string:
search for message = {esc}s;
  #  repeat search for next message with same search string:
search message     = {esc}s {enter};

  # <<<>>> need way to narrow rmail-buffer afterwards to right message
wide search        = MessageBuffer() {ctrl+x}nw  Do(mdl-occur);



### 
### Simple operations on message(s) in the current folder:
### 

  # go to prompted-for absolute message number
go to message              =  Do(mdl-rmail-show-message);


  # go to nearest message whose message number equals r mod 100 from
  # Rmail or summary buffers:
Message(r) := h LineMod($r);
                  # work around bug with wb-line-number.el:  <<<>>>
              #. {ctrl+x}o {esc}<;

message <r>                = Message($1);
message <r> <action>       = Message($1)            $2;
message <r> <action> 2..10 = Message($1) Repeat($3, $2);

<once>       message       =            $1;

<rep>        message       =            $1;
<rep>  2..20 messages      = Repeat($2, $1);
<rep>        message 2..20 = Repeat($2, $1);


<once>   := ( first = < | last = > | star = a*{enter} | edit = e );
<rep>    := ( next = n | previous = p
            | delete = d | delete backwards = {ctrl+d}
	    );
<action> := ( delete = d | delete backwards = {ctrl+d} );


  #  move to top of message body then lift it to top of screen:
message body   = MessageBuffer() 
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
## Filing messages:
## 

  # send current email message to its default folder:
             file that                 =            o{enter};
prefix 1..20 file that                 = {ctrl+u}$1 o{enter};

             file folder               =            o NoCaps();
             file <Rmail_folder>       =            o Line(Base() /$1);
prefix 1..20 file <Rmail_folder>       = {ctrl+u}$1 o Line(Base() /$2);


file <Rmail_folder> and <Rmail_folder> =
          # this works only if current message not followed by a deleted message:
        "{esc}:" '(rmail-output-to-rmail-file "' Base() '/$1" 1)' {enter}
        u o Line(Base() /$2);

file in <Rmail_directory> folder = o EraseToStart() Base() /$1/ NoCaps();

  # experiment:  <<<>>>
file from <personal_folder>      = o EraseToStart() Base() /$1 {enter};


## 
## Opening mail folders:
## 

Open()     := Do(rmail-input);

open folder                     = Open() NoCaps();
open <Rmail_folder>             = Open() Line(Base() /$1);
open <Rmail_folder> summary     = Open() Line(Base() /$1)  Summarize();

open a <Rmail_directory> folder = Open() EraseToStart() Base() /$1/ NoCaps();


open from <personal_folder>     = Open() Line(Base() /$1);

  # experiment; probably doesn't always work:  <<<>>>
open work <work_email>          = Open() 
       Line(Base() /h/ Replace(Replace($1, "@hp.com", ""), "_", ".") );



### 
### Dealing with MIME:
### 

include "URLs.vch";

ExportMIME() := MessageBuffer()
                {ctrl+u}0t		    # show all headers
		Do(mdl-export-buffer-to-l1)
	 	t; 			    # prune headers

  # munpack current buffer to ~/Tmp/Export:
UnpackBuffer() := ExportMIME()
	          Shell("ruby ~/bin/unpack_l1.rb");
	       

## 
## Viewing MIME messages:
## 

  # munpack current buffer to ~/Tmp/Export then view that directory:
[mime] unpack buffer =
	UnpackBuffer()
	AppBringUp("Export", "explorer /select," UNIXfromPC(~/Tmp/Export/l1))
	Wait(2000)
	{ctrl+r}{ctrl+g};

view HTML =
	UnpackBuffer()
	   # lookup text html part or whole message if none:
	Lookup(UNIXfromPC(~/Tmp/Export/_html.html));

view meeting request =
	UnpackBuffer() Wait(1000)
	AppBringUp("meeting", UNIXfromPC(~/Tmp/Export/_meeting.ics));

view text =
	UnpackBuffer()
	   # lookup (text text part or whole messages if none) plus contents:
	{ctrl+x}b *text* {enter}
	{esc}< {esc}> {ctrl+w}
	"{esc}:(longlines-mode 0){enter}"
   	   {ctrl+x}i Line(~/Tmp/Export/_text.txt)
	"{esc}:(longlines-mode 1){enter}";

## 
## Replying to and forwarding MIME messages:
## 

reply to text =
	UnpackBuffer()
	   # lookup text part or whole message if none:
	r{enter}
	{ctrl+c}{ctrl+o}
	{ctrl+x}i Line(~/Tmp/Export/_text.txt)
	{ctrl+x}1
	Leap(d, "=======") {down} {ctrl+w};


GoTo()                 := {esc}< Leap(D+, "To: ");
GoSubject()            := {esc}< Leap(D+, "Subject:");

ForwardMIME(addresses) := {ctrl+u}0t                   # show all headers
                          Do(mime-forward)
		          GoTo() Replace($addresses, ",", ", ")
			  GoSubject() {right};

forward MIME message                          = ForwardMIME("") GoTo();
forward MIME message to      <personal_email> = ForwardMIME($1);
forward MIME message to work <work_email>     = ForwardMIME($1);
