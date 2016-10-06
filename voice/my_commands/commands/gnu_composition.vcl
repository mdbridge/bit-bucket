###
### Gnuemacs voice commands for preparing and sending email using message-mode
###

include "gnu.vch";

include "directories.vch";


  # insert prefixed message body of message being replied to:
#YankBody() := {ctrl+c}{ctrl+y} {ctrl+u}7{ctrl+l};
# <<<>>>
YankBody() := {ctrl+c}{ctrl+y} Do(mdl-wrap-long-lines) {ctrl+u}7{ctrl+l};



##
## Starting to compose a message:
##

GoTo()      := {esc}< Leap(D+, "To: ");
GoSubject() := {esc}< Leap(D+, "Subject:");

# 
# Standard commands to compose and forward messages:
# 

Compose(addresses) := Do(message-mail)
		        # add a blank line at the end of the message
		      {esc}> {enter} GoTo()
		      Replace($addresses, ",", ", ")
		      GoSubject() {end};

Forward(addresses) := F
		      Replace($addresses, ",", ", ")
		      GoSubject() {right};

include "mail.vch";


# 
# Other recipients:
# 

compose message = Compose("")  GoTo();
forward message = Forward("")  GoTo();
#forward {[as]seen,text,HTML} in x_mail.vcl


# 
# Replying to messages:
# 

Reply() := R {esc}> {enter};

reply to sender  = {ctrl+u}1 Reply()            {ctrl+c}{ctrl+o};
reply to all     =           Reply()            {ctrl+c}{ctrl+o};

  # these reply to all:
reply to message =           Reply()            {ctrl+c}{ctrl+o};   
reply to body    =           Reply() YankBody() {ctrl+c}{ctrl+o};
#reply to text in x_mail.vcl


# 
# Compose with initial message body (overrides _any.vcl):
# 

include "switch.vch";

<clipboard> := ( clipboard="" | selection={esc}w | region={esc}w );

<context_mail> <clipboard> to      <personal_email> = $2 MailClipboard($1,$3);
<context_mail> <clipboard> to work <work_email>     = $2 MailClipboard($1,$3);



## 
## Changing the subject:
## 

new    subject line = GoSubject() " " {ctrl+k};
change subject line = GoSubject() " [WAS:" {end} "]" GoSubject() " ";


## 
## Changing recipients:
## 

AddCC(recipient) := SaveExcursion(GoSubject() {home}{ctrl+o} "CC: $recipient");

add CC to clipboard             = AddCC({ctrl+y});
add CC to      <personal_email> = AddCC($1);
add CC to work <work_email>     = AddCC($1);

  # Remove "To: <...>" and promote CC to TO:
not to me = SaveExcursion({ctrl+c}{ctrl+f}t);


## 
## Miscellaneous field changes:
## 

change importance = {ctrl+c}{ctrl+u};  # change priority
sort headers      = {ctrl+c}{ctrl+o};


## 
## Adding attachments:
## 

Add_Attach(prefix) := {ctrl+c}{ctrl+a} EraseToStart() $prefix;

add attachment = {ctrl+c}{ctrl+a};

add attachment from [<D>] <UNIX> [/ <COM>] = Add_Attach(UNIX($1$2 When($3,/$3) /));
add attachment from       <PC>   [/ <COM>] = Add_Attach(PC(    $1 When($2,/$2) /));

attach buffer = {ctrl+c}{enter}b {tab};  # experimental <<<>>>



##
## Sending messages:
##

  # send currently being composed message, removing composition window:
send     message = {ctrl+c}{ctrl+c} {ctrl+x}o {ctrl+x}1;

#postpone message = {ctrl+c}{ctrl+d};
#discard  message = {ctrl+c}{ctrl+k};


  # send short thank you note to sender of current message only
please thank sender =
	{ctrl+u}1R
	{enter}Thanks!{enter}{enter}
	"- Mark{enter}"
	{ctrl+c}{ctrl+c};

wrong email address =
	{ctrl+u}1R
	{enter}
	{ctrl+x}1
	" and send your"{enter}
	"message to the right email address.  Do not send more email"
	" to this address."
	{enter}{enter}
	"- Mark{enter_3}"
	{ctrl+c}{ctrl+y};


##
## Inserting (some of) message being replied to:
##

  # insert message that is being replied to:
quote message      = {ctrl+u}{ctrl+c}{ctrl+y}
	  	     Do(mdl-insert-string) ">  "   {enter};
		     
quote message body = YankBody();

# message indent <range>


CommandOther(operator, selector) :=
	{ctrl+x}o SavePoint(ScratchRegister())
	$selector
	Replace($operator, "%", RestorePoint(ScratchRegister()) {ctrl+x}o);

  # other [line N1, line N2)
mail yank <r> comma <r> = 
          CommandOther("{esc}w%", LineMod($1) Mark() LineMod($2))
	  {ctrl+y}
	  Do(mdl-insert-string) ">  "   {enter};

mail yank <r> onwards   = 
	  CommandOther("{esc}w%", LineMod($1) {esc}>)
	  {ctrl+y}
	  Do(mdl-insert-string) ">  "   {enter};



## 
## Editing a quoted message:
## 

  # fill result of quote message [body]:
justify message = {ctrl+c}{ctrl+q};

#cut open <r>    = LineMod($1) {ctrl+k} {enter} {ctrl+o_2};
cut open <r>    = LineMod($1) Mark()
     	 	  LeapRegexRaw(D+, '\(^[> {ctrl+q}{ctrl+i}]*{ctrl+q}{ctrl+j}\)*')
		  {ctrl+w} {enter} {ctrl+o_3};

cut tail <r>    = LineMod($1) {esc}> {ctrl+w} {enter} 
                           {ctrl+u}-4{ctrl+l};

  # move to point to split at (anywhere inside spaces if any) in message indented text then:
split message = {ctrl+u}0{esc}{space} {enter_5} "> " {up_3};



## 
## Miscellaneous message edits:
## 

# Insert a cut here line, with cursor in the right place to insert 
# "for file foo":
(insert cut here|cut here line|cut line here) =
	{home}
	"====================  cut here  ===================="
	{ctrl+o} Leap(u, ' ');
