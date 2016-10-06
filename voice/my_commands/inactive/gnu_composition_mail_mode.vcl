###
### Gnuemacs macros for preparing and sending email using mail mode
###

include "gnu.vch";

include "directories.vch";


  # insert prefixed message body of message being replied to:
YankBody() := {ctrl+c}{ctrl+y}
	      {ctrl+x}{ctrl+x} {ctrl+space} LeapRegex(D, "^>  $")
	      {home} {down} {ctrl+w}
	      {ctrl+u}5{ctrl+l};


##
## Starting to compose a message:
##

GoTo()      := {esc}< Leap(D+, "To: ");
GoSubject() := {esc}< Leap(D+, "Subject:");

# 
# Standard commands to compose and forward messages:
# 

Compose(addresses) := Do(mail)
		        # add a blank line at the end of the message
		      {esc}> {enter} GoTo()
		      Replace($addresses, ",", ", ")
		      GoSubject() {end};

Forward(addresses) := f
		        # add a blank line before the forwarded message:
		      Leap(D, -----) {ctrl+a} {enter} GoTo()
		      Replace($addresses, ",", ", ")
		      GoSubject() {right};

include "mail.vch";


# 
# Other recipients:
# 

compose message      = Compose("")  GoTo();
forward message      = Forward("")  GoTo();


# 
# Replying to messages:
# 

reply to sender  = {ctrl+u}1 r{enter};
reply to all     =           r{enter};
reply to message =           r{enter};   # this replies to all

reply to body    =           r{enter} YankBody();
#reply to text in x_mail.vcl  # <<<>>>


# forward MIME message ... in x_mail.vcl  # <<<>>>


# 
# Compose with initial message body (overrides _any.vcl):
# 

include "switch.vch";

<clipboard> := ( clipboard="" | selection={esc}w | region={esc}w );

mail <clipboard> to      <personal_email> = $1 MailClipboard($2);
mail <clipboard> to work <work_email>     = $1 MailClipboard($2);



## 
## Changing the subject:
## 

new    subject line = GoSubject() " " {ctrl+k};
change subject line = GoSubject() " [WAS:" {end} "]" GoSubject() " ";



## 
## Changing recipients:
## 

add CC to      <personal_email> = 
        SaveExcursion(GoSubject() {home}{ctrl+o} "CC: $1");
add CC to work <work_email>     = 
	SaveExcursion(GoSubject() {home}{ctrl+o} "CC: $1");


  # Remove "To: <...>" and promote CC to TO:
not to me = GoTo() {home} {ctrl+k_2}
            {esc}< LeapRegex(D, '^CC:') {Del_2} To {right_2};



## 
## Adding attachments:
## 

Add_Attach(prefix) := Do(attach) EraseToStart() $prefix;

add attachment = Do(attach);

add attachment from <UNIX>                 = Add_Attach(UNIX($1      /));
add attachment from <UNIX> / <COM>         = Add_Attach(UNIX($1/$2   /));
add attachment from <UNIX> / <COM> / <COM> = Add_Attach(UNIX($1/$2/$3/));



##
## Sending messages:
##

  # send currently being composed message, removing composition window:
send message        = {ctrl+c}{ctrl+c} {ctrl+x}o {ctrl+x}1;

  # send short thank you note to sender of current message only
please thank sender =
	{ctrl+u}1r
	{enter}Thanks!{enter}{enter}
	"- Mark{enter}"
	{ctrl+c}{ctrl+c};



##
## Inserting (some of) message being replied to:
##

  # insert message that is being replied to:
quote message      = {ctrl+c}{ctrl+y} {ctrl+x}{ctrl+x};
quote message body = YankBody();

# message indent <range>


CommandOther(operator, selector) :=
	{ctrl+x}o SavePoint(ScratchRegister())
	$selector
	Replace($operator, "%", RestorePoint(ScratchRegister()) {ctrl+x}o);

  # other [line N1, line N2)
mail yank <r> comma <r> = 
          CommandOther("{esc}w%", LineMod($1) {ctrl+space} LineMod($2))
	  {ctrl+y}
	  Do(mdl-insert-string) ">  "   {enter};

mail yank <r> onwards   = 
	  CommandOther("{esc}w%", LineMod($1) {ctrl+space} {esc}>)
	  {ctrl+y}
	  Do(mdl-insert-string) ">  "   {enter};



## 
## Editing a quoted message:
## 

  # fill result of quote message [body]:
justify message = {ctrl+c}{ctrl+q};

cut open <r>    = LineMod($1) {ctrl+k} {enter} {ctrl+o_2};
cut tail <r>    = LineMod($1) {ctrl+space}{ctrl+end}{ctrl+w} {enter} 
                           {ctrl+u}-4{ctrl+l};



## 
## Miscellaneous message edits:
## 

# Insert a cut here line, with cursor in the right place to insert 
# "for file foo":
insert cut here =
	{home}
	"====================  cut here  ===================="
	{ctrl+o} Leap(u, ' ');
