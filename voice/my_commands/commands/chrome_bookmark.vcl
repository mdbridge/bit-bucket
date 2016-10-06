### 
### Voice commands for Chrome (version 51) dealing with bookmarks and cookies
### 

include "locale_PC.vch";
include "chrome.vch";

FixFocus() := Address() UnAddress();

AwaitChange(actions) :=
    Variable.Set(:target, Window.ID())
    $actions
    Repeat(50, 
        If(Window.Match(ID> Variable.Get(:target)), Wait(100)));


## 
## Creating bookmarks:
## 

BookmarkThat0(prefix) := Address() {ctrl+d} Wait(500) {home} $prefix
                        {tab} Upload {shift+tab};

  # add current URL to list of links to upload to email:
BookmarkThat(prefix)  := BookmarkThat0($prefix) AwaitChange({enter})
		      	 FixFocus();

BookmarkTitle(prefix) := {ctrl+c} BookmarkThat0({ctrl+a}{backspace}
						$prefix{ctrl+v});


<bookmark> := ( work bookmark=W | personal bookmark=P | home bookmark=P 
              |      bookmark=IfHome(P,W) );
<thing>    := ( paper = PAPER | slides = SLIDES | video = VIDEO );


<bookmark>  that	      = BookmarkThat("[$1] ");
<bookmark> [that] as read     = BookmarkThat("[$1][R] ");

  # select title of <thing> before issuing these commands:
<bookmark> <thing>	      = BookmarkTitle("[$1] $2: ");
<bookmark> <thing> as read    = BookmarkTitle("[$1][R] $2: ");
<bookmark> <thing> as printed = BookmarkTitle("[$1][pri] $2: ");



##
## Commands for organizing bookmarks:
##

include "home_bookmark_folders.vch";
include "work_bookmark_folders.vch";

organize (favorites|bookmarks) = Address() {ctrl+shift+o};

#Library:
#  file <home_bookmark_folder> = File($1);
#  file <work_bookmark_folder> = File($1);
#
#    # first select the folder you wish to sort:
#  sort by name = {shift+f10}r;
#:



## 
## Exporting bookmarks:
## 

import bookmarks = {alt+f}b i;


Organize() := OpenNewURL("chrome://bookmarks/#1") Wait(500) {tab_3};

organize menu = Organize();

Export0()    := Organize() {alt+down} {up} {enter} WaitForWindow("Save As");
Export(path) := Export0() $path Wait(100) {enter};


manually export bookmarks = Export0();

	 export bookmarks = 
    IfHome(Export(UNIX(foil:~/backups/bookmarks/Chrome-home.htm)),
	   Export(UNIX(work:~/backups/bookmarks/Chrome-work.htm)));


upload links                = Export(UNIX(   ~/http/Chrome-bookmarks.htm));
upload links to (foil|work) = Export(UNIX($1:~/http/Chrome-bookmarks.htm));  
manually upload links       = Export(PC(~/scratch/Chrome-bookmarks.htm));  # <<<>>>



## 
## Backing up bookmarks:
## 

#push bookmarks = 
#    ImportAndBackup(b) WaitForWindow("Bookmarks backup filename", "", 40000)
#    Wait(100) {ctrl+c} UNIX(~/backups/bookmarks/) {ctrl+v};
#
#pull bookmarks =
#    ImportAndBackup(rc) WaitForWindow("Select a bookmarks backup") 
#    Wait(100) UNIX(~/backups/bookmarks/) Wait(100) {enter};



## 
## Exporting cookies (via cookie exporter 1.5 add-in):
## 

#UploadCookies(location) := Address() {alt+t} Wait(100) e WaitForWindow("Save As")
# 			   UNIX($location ~/http/cookies.txt) {enter}; 
#
#upload cookies [to (foil|work)] = UploadCookies(When($1,"$1:"));
