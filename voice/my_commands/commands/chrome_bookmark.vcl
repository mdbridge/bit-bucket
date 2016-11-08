### 
### Voice commands for Chrome (version 54) dealing with bookmarks and cookies
### 

include "locale_PC.vch";
include "chrome.vch";
include "import.vch";

FixFocus() := Address() UnAddress();

  # <<<>>>
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

#
# Keyboard commands for organizing bookmarks are crude at best.
# Doesn't seem worth trying to make commands for this.
#



## 
## Exporting bookmarks:
## 

Organize() := OpenNewURL("chrome://bookmarks/#1") Wait(700) {tab_3};

organize menu = Organize();

Export0()     := Organize() {alt+down} {up} {enter} 
 	         WaitForWindow("Save As") Wait(1000);
Export1(path) := Export0() AwaitChange( $path{enter} );

Export(target, name) :=
    PrepareUpload($target)
    Export1( $name {home} UploadDir() \ )
    DoUpload();


manually export bookmarks = Export0();
	 export bookmarks = 
	     Export(@~/backups/bookmarks, Chrome- IfHome(home,work) .htm);

upload links [to (foil|work)] = 
    Export(@ When($1,"$1:","") ~/http, Chrome-bookmarks.htm);

manually upload links = Export1(PC(~/scratch/outgoing/Chrome-bookmarks.htm));  # <<<>>>



## 
## Backing up bookmarks:
## 

push bookmarks = Export(@~/backups/bookmarks, {ctrl+c} Chrome- {ctrl+v});

# could also do this via {alt+f}b i...
pull bookmarks = Organize() {alt+down} {up_2} {enter} WaitForWindow("Open");
