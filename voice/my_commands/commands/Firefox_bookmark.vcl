### 
### Voice commands for Firefox (version 47.0) dealing with bookmarks and cookies
### 

include "locale_PC.vch";
include "Firefox.vch";
include "import.vch";

FixFocus() := Address() UnAddress();

  # <<<>>>
AwaitChange(actions) :=
    Variable.Set(:target, Window.ID())
    $actions
    Repeat(50, 
        If(Window.Match(ID> Variable.Get(:target)), Wait(100)));


##
## Going to Bookmarks:
##

# {view, hide} bookmarks sidebar; sidebar has search functionality

# click bookmarks; click <X>...



## 
## Creating bookmarks:
## 

BookmarkThat0(prefix) := Address() {ctrl+d} Wait(500) {home} $prefix
                       {alt+n}{tab} Upload {alt+n};

  # add current URL to list of links to upload to email:
BookmarkThat(prefix)  := BookmarkThat0($prefix) {alt+d} FixFocus();

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

File(folder) := {alt+o} Wait(100) m WaitForWindow("Choose Folder") 
	     	Split( $folder,  ":",0) {right} Wait(1000)
		Split("$folder:",":",1) {tab_2};# {enter};  # <<<>>>


organize (favorites|bookmarks) = Address() {ctrl+shift+b};

Library:
  file <home_bookmark_folder> = File($1);
  file <work_bookmark_folder> = File($1);

    # first select the folder you wish to sort:
  sort by name = {shift+f10}r;
:



## 
## Exporting bookmarks:
## 

Library() := Address() {ctrl+shift+b} WaitForWindow(Library) Wait(100);

ImportAndBackup(letter) := Library() {alt+i} Wait(1000) $letter;

Export0()    := ImportAndBackup(e) 
	        WaitForWindow("Export Bookmarks File", "", 20000) Wait(2000);
  # this is more fragile than I would like...  <<<>>>
Export1(path) := Export0() AwaitChange( $path Wait(500) {enter} );

Export(target, name) :=
    PrepareUpload($target)
    Export1( $name {home} UploadDir() \ )
    DoUpload();


manually export bookmarks = Export0();
	 export bookmarks = 
	     Export(@~/backups/bookmarks, Firefox- IfHome(home,work) .htm);

upload links [to (foil|work)] = 
    Export(@ When($1,"$1:","") ~/http, Firefox-bookmarks.htm);

manually upload links = Export1(PC(~/scratch/outgoing/Firefox-bookmarks.htm));  # <<<>>>



## 
## Backing up bookmarks:
## 

push bookmarks = 
    PrepareUpload(@~/backups/bookmarks)
    ImportAndBackup(b) WaitForWindow("Bookmarks backup filename", "", 40000)
    Wait(100) AwaitChange( {ctrl+c} UploadDir()\ {ctrl+v} Wait(500) {enter} )
    DoUpload();

pull bookmarks =
    ImportAndBackup(rc) WaitForWindow("Select a bookmarks backup") 
    Wait(100) UNIX(~/backups/bookmarks/) Wait(100) {enter};



## 
## Exporting cookies (via cookie exporter 1.5 add-in):
## 

UploadCookies(location) := Address() {alt+t} Wait(100) e WaitForWindow("Save As")
 			   UNIX($location ~/http/cookies.txt) {enter}; 

upload cookies [to (foil|work)] = UploadCookies(When($1,"$1:"));
