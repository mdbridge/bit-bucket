###
### Voice commands for Microsoft Outlook 2013
###

include "office.vch";

include "string.vch";


Mail()     := {ctrl+1};
Contacts() := {ctrl+3};

Ribbon(keys) := {alt+$keys};

File() := Ribbon(f);
Home() := Ribbon(h);



### 
### Global commands:
### 

  # missing from Dragon for some reason:
Click File = File();

export contacts = Contacts()
                  File() o i WaitForWindow("Import and Export Wizard")
	          {up_10}{down}{enter} Wait(100) {enter};


##
## Starting to compose a message:
##
##   Below assumes show BCC option has been selected (under message options)
##

To(addresses) := {alt+u} Wait(100) {shift+tab_3}
                 Replace($addresses, ",", "; ");

BlindCopy()   := {alt+u}{shift+tab}
                 mark.lillibridge@hpe.com{tab};

Subject()     := {alt+u};
  # the below doesn't actually work for new meetings: <<<>>>
Body()        := {alt+u}{tab};


# 
# Standard commands to compose and forward messages:
# 

Compose(addresses) := {ctrl+shift+m}   To($addresses) BlindCopy() Subject();

Forward(addresses) := {ctrl+f}{ctrl+o} To($addresses) BlindCopy() Body();

include "mail.vch";


# 
# Other recipients:
# 

# user-specified:

compose message = {ctrl+shift+m}   BlindCopy() To("");
forward message = {ctrl+f}{ctrl+o} BlindCopy() To("");

  # attaches a RFC822 message; "message body" doesn't work from result...
forward as attachment = {alt+ctrl+f} BlindCopy() To("");



## 
## Switching to given folders:
## 

# note that these override some built-in Dragon commands that may? crash Outlook

<folder> := ( attachments 
            | review
	    | "Deleted Items"
            | Inbox
            |   hold              = Inbox {right} hold
            | invitations
            | "Junk E-mail" 
            | "Sent Items"
            | unread mail         = "Search Folders{right}Unread Mail"
            | unread email        = "Search Folders{right}Unread Mail"
            | tube results        = "Tube results{right}tube results"
            | Embarcadero results = "Tube results{right}Embarcadero results"

            | Mail                = Inbox
	    | Calendar 
	    | Contacts | people=Contacts
	    );

  # this doesn't work from within a in-line message body (use {ctrl+o} to pop out):
GoFolder(name) := {ctrl+y} $name {enter};

show <folder> = GoFolder($1);

include "letters.vch";

show contacts <letter> = Contacts() {shift+$1};



### 
### Mail view:
### 

EmailList() := {f3}{esc};   # move focus to list of emails

email list = EmailList();  # also undoes current search


Search(target) := {ctrl+e} {ctrl+a} $target Wait(1000) {tab_4};

<type> := ( Subject="Subject: " | sender="From: " | keyword="");

  # need tab 4 afterwards...  <<<>>>
filter by <type>	     = Search($1 Vocola.Abort());

filter by [<type>] phrase <_anything> = Search($1$2);

filter by sender work <work_email>     = Search("From: $1");
filter by sender      <personal_email> = Search("From: $1");


reply to (message={ctrl+shift+r}|sender={ctrl+r}) =
    $1 {ctrl+o} WaitForWindow("*- Message*")  BlindCopy() Body();

  # experimental; not sure does BCC to me:
reply with meeting request = {alt+ctrl+r};


star message = Home()g {down_2}{enter};

  # appear to need "dictate XXX" to specify a folder here sometimes:
file that    = {ctrl+shift+v} hold;  # hold is only default...


# empty deleted items folder



### 
### In a composing message window:
### 

message body = Body();



### 
### In a message window (e.g., hit enter when in email list):
### 

# {esc} dismisses an open email window

# open [the] {first..nineth} attachment
# open attachment 1..9

  # built-ins don't work with calendar emails:
(next={ctrl+.}|previous="{ctrl+,}") message = $1;

info bar          = {ctrl+shift+w};
  # does this work with 2013?  <<<>>>
download pictures = {ctrl+shift+w}p;

display blocked content = {ctrl+shift+i};



### 
### Calendar view:
### 

# click {today,next 7 days}

# click {day/work week/week/month}

1..9 days = {alt+$1};
10   days = {alt+0}; 


(previous=left | next=right) day   = {ctrl+$1};
(previous=up   | next=down)  week  = {alt+$1};
(previous=PgUp | next=PgDn)  month = {alt+$1};



<month> := ( January=1 | February=2 | March=3 | April=4 | May=5 | June=6
	   | July=7 | August=8 | September=9 | October=10 | November=11
	   | December=12 );

  # date here is via # not 'st (one not first!)
go <month> [1..31] [2000..2030] =
    {ctrl+g} $1/ When($2,$2,1) / When($3,$3,Date.Now("%Y")) {enter};

go 2000..2030 = {ctrl+g} Date.Now("%m/%d/$1" ) {enter};


# new {meeting request, appointment): highlight time range first (shift...)

# can use mouse to drag or resize appointments

# (shift-)F9 refreshes calendar view

# changing the "show as" field can make other people's FTO not show me as busy


## 
## Experiment:
## 

# at work, Outlook maximized on right monitor, work week:

Move(hourX2, day) := Mouse.Go(Eval(1883 + $day*260 + 20),
	     	              Eval(398 +  $hourX2*23 - 16*23 + 5));


<action> := ( hover="" | touch=Mouse.Click() | open=Mouse.Click() {enter} );
<day>	 := ( Monday=0 | Tuesday=1 | Wednesday=2 | Thursday=3 | Friday=4);

<action> <day> 6..11        A M = Move(Eval($3*2), $2) $1;
<action> <day> 6..11 thirty A M = Move(Eval($3*2+1), $2) $1;
<action> <day> 12           P M = Move(Eval(     12*2), $2) $1;
<action> <day> 12    thirty P M = Move(Eval(     12*2+1), $2) $1;
<action> <day> 1..8         P M = Move(Eval($3*2+12*2), $2) $1;
<action> <day> 1..8  thirty P M = Move(Eval($3*2+12*2+1), $2) $1;




### 
### Contacts view:
### 

# click {people, business card, ...}

  # this requires the business card view:
  # select a contact in contacts display then:
copy address = {ctrl+o} Wait(1000) {alt+i}{shift+tab} {ctrl+a}{ctrl+c} {esc};
