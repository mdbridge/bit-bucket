###
### Voice commands for Microsoft Outlook 2007
###

include "string.vch";



##
## Starting to compose a message:
##
##   Below assumes show BCC option has been selected (under message options)
##

To(addresses) := {alt+u} Wait(100) {shift+tab_3}
                 Replace($addresses, ",", "; ");

BlindCopy()   := {alt+u}{shift+tab}
                 mark.lillibridge@hp.com{tab};

Subject()     := {alt+u};
Body()        := {alt+u}{tab};


message body = Body();


# 
# Standard commands to compose and forward messages:
# 

Compose(addresses) := {alt+f}wm To($addresses) BlindCopy() Subject();

Forward(addresses) := {alt+w}   To($addresses) BlindCopy() Body();

include "mail.vch";


# 
# Other recipients:
# 

# user-specified:

compose message = {alt+f}wm BlindCopy() To("");
forward message = {alt+w}   BlindCopy() To("");

reply to message = {alt+l} WaitForWindow("*- Message*")  BlindCopy() Body();
reply to sender  = {alt+r} WaitForWindow("*- Message*")  BlindCopy() Body();



## 
## Switching to given folders:
## 

# note that these override some built-in Dragon commands that crash Outlook

<folder> := ( attachments 
            | review
	    | "Deleted Items"
            | Inbox
            |   hold              = "Inbox\hold"
            | invitations
            | "Junk E-mail" 
            | "Sent Items"
            | unread mail         = "Search Folders\Unread Mail"
            | unread email        = "Search Folders\Unread Mail"
            | tube results        = "\\Tube results\tube results"
            | Embarcadero results = "\\Tube results\Embarcadero results"

            | Mail                = Inbox
	    | Calendar | Contacts
	    );

  # this crashes Outlook without the wait...
GoFolder(name) := {ctrl+y}{tab_3} $name Wait(500) {enter};

show <folder> = GoFolder($1);

include "letters.vch";

show contacts <letter> = {ctrl+3}{shift+$1};



## 
## Miscellaneous:
## 

# empty deleted items folder

star message = {alt+a}i{down_3}{enter};

file that    = {ctrl+shift+v} Embarcadero;  # Embarcadero is only default...



  # select a contact in contacts display than:
copy address = {ctrl+o} Wait(1000) {alt+i}{shift+tab} {ctrl+a}{ctrl+c} {alt+f4};

EmailList() := # {ctrl+1}
               {f3}{esc};

  # move focus to list of emails:
email list = EmailList();



## 
## Calendar view:
## 

# today

# day/week/month

1..9 days = {alt+$1};
10   days = {alt+0}; 


(previous=up   | next=down) week  = {alt+$1};
(previous=PgUp | next=PgDn) month = {alt+$1};



<month> := ( January=1 | February=2 | March=3 | April=4 | May=5 | June=6
	   | July=7 | August=8 | September=9 | October=10 | November=11
	   | December=12 );

go <month> [1..31] [2000..2030] =
    {ctrl+g} $1/ When($2,$2,1) / When($3,$3,Date.Now("%Y")) {enter};

go 2000..2030 = {ctrl+g} Date.Now("%m/%d/$1" ) {enter};



## 
## Message window:
## 

# open [the] {first..nineth} attachment
# open attachment 1..9

# {next,previous} message

view in browser = {alt+h}xv;

# {esc} dismisses an open email window

info bar          = {ctrl+shift+w};
download pictures = {ctrl+shift+w}p;
