### 
### Voice commands for web browsers (common implementations)
### 
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
### 

include "locale_PC.vch";



##
## Going to Bookmarks:
##

HomePage() := https://www.google.com/;

#go home = Address() {alt+home};  # fails on IE 11 without long wait
go home = OpenURL(HomePage());


include "web_sites.vch";
include "work_sites.vch";

    Web  <web_site>  =    OpenURL($1);
    Work <work_site> =    OpenURL($1);

new Web  <web_site>  = OpenNewURL($1);
new Work <work_site> = OpenNewURL($1);


(home|work) accelerators = OpenURL(file:/// PC(~/scratch/$1-accelerators.html));

include "home_bookmark_folders.vch";
include "work_bookmark_folders.vch";

go <home_bookmark_folder> bookmarks = 
    OpenURL(file:/// PC(~/scratch/home-accelerators.html) "#$1");
go <work_bookmark_folder> bookmarks = 
    OpenURL(file:/// PC(~/scratch/work-accelerators.html) "#$1");



## 
## Going to directories:
## 

include "navigate.vch";



## 
## Dealing with addresses:
## 

go to address      = Address();

copy (address|URL) = Address() {ctrl+c} UnAddress();

  #  go to URL stored in the clipboard:
    import URL =    OpenURL({ctrl+v});
new import URL = OpenNewURL({ctrl+v});

force import URL =    OpenURL(Clipboard.Get());


  # open a new tab with the current URL:
clone URL      = Address() {alt+enter};


include "switch.vch";

re-open [that] in <browser> = 
	Address() {ctrl+c} UnAddress() Wait(100)
	BringUpBrowser($1)
	Wait(3000)
	HeardWord(import, URL);



## 
## Navigation:
## 

  # Attempt to fix problem where focus is often left on the address
  # bar instead of on the document were it belongs:
FixFocus() := Address() UnAddress();


# 
# Within a page:
# 

  #  go to top of page then refresh:
refresh top = {ctrl+home} {ctrl+r};


# 
# Within history of pages:
# 

GoFwd (count) := Address() SendSystemKeys({alt+right_$count}) Wait(100) FixFocus();
GoBack(count) := Address() SendSystemKeys({alt+left_$count})  Wait(100) FixFocus();

         go forward [1..10] = GoFwd (When($1,$1,1));
[really] go back    [1..10] = GoBack(When($1,$1,1));


# 
# Across tabs:
# 

  # Dragon limits these to bizarre places:
#open new tab = Address() {ctrl+t};
open [new]  tab = OpenNewURL(HomePage());  # workaround for new tab not working with FF 4 <<<>>>
reopen      tab = {ctrl+shift+t};
close       tab = Address() {ctrl+w};

close 1..10 tabs = Address() Repeat($1, {ctrl+w} Wait(100));


Minus(n) := {ctrl+9} Repeat(Eval($n-1), Address() {ctrl+shift+tab});

<op> := ( refresh = {ctrl+r} | reload = {ctrl+r} | close = {ctrl+w} );

window		1..9 [<op>] = Address() {ctrl+$1} When($2,Address() $2) FixFocus();
window minus	1..9 [<op>] = Address() Minus($1) When($2,Address() $2) FixFocus();
window negative 1..9 [<op>] = Address() Minus($1) When($2,Address() $2) FixFocus();


<dir> := ( next = {ctrl+tab} | previous = {ctrl+shift+tab} );

<dir> window 2..9   = Repeat($2, Address() $1) FixFocus();
<dir> window [<op>] = Address() $1 When($2,Address() $2) FixFocus();



## 
## Sending URLs via email:
## 

include "email_addresses.vch";
include "switch.vch";

GoTo() := {esc}< {ctrl+s}To:{enter} {end};

MailURL(mail_context, recipients, subject) := 
        Address() {ctrl+c} UnAddress()
	MailClipboard($mail_context, $recipients)
	$subject  When($recipients, "", GoTo());

MailHeadline(mail_context, recipients) :=
	Clipboard.Set("xyzzy") {ctrl+c} Clipboard.WaitForNew("xyzzy",5)
	  Clipboard.Save(_headline)
	MailURL($mail_context, $recipients,
	         Wait(3000) Clipboard.Restore(_headline) {ctrl+y});

<context_mail> URL      [to      <personal_email>] = MailURL($1, $2, "");
<context_mail> URL       to work <work_email>      = MailURL($1, $2, "");

<context_mail> headline [to      <personal_email>] = MailHeadline($1, $2);
<context_mail> headline  to work <work_email>      = MailHeadline($1, $2);



##
## Miscellaneous:
##

save page = {ctrl+s};

Print	  = Beep();    # stop misrecognitions...

Copy That = {ctrl+c};  # Sometimes failed to be recognized


define phrase <_anything> = 
    OpenURL("https://www.google.com/search?q=define%3A+$1");

Google(text) := OpenNewURL(https://www.google.com/) Wait(1000) $text {enter};
#Google(text) := OpenNewURL(https://encrypted.google.com/) Wait(1000) $text {enter};
#Google(text) := OpenNewURL(https://encrypted.google.com/) Wait(1000) 
#                Address() {tab_7} $text {enter};

Google clipboard          = Google({ctrl+v});
Google phrase <_anything> = Google($1);

plus phrase <_anything> = 
    OpenNewURL("http://en.cppreference.com/mwiki/index.php?search=" 
    	       Replace($1," ","+"));
plus search <_anything> = 
    OpenNewURL("http://en.cppreference.com/mwiki/index.php?search=" 
    	       Replace($1," ","+"));

