###
### Commands for Internet Explorer (version 11) dealing with bookmarks
###
###   Do not create any IE folders starting with "u" other than Upload!
###

include "locale_PC.vch";
include "switch.vch";
include "iexplore.vch";
include "import.vch";

  # <<<>>>
AwaitChange(actions) :=
    Variable.Set(:target, Window.ID())
    $actions
    Repeat(50, 
        If(Window.Match(ID> Variable.Get(:target)), Wait(100)));



##
## Going to Bookmarks:
##

# switch to favorites

# click favorites; click <X>...



## 
## Creating bookmarks:  [DISABLED]
## 

  # add current URL to list of links to upload to email:
#BookmarkThat(prefix) := Address() {ctrl+d} 
#		        WaitForWindow("Add a Favorite") 
#		        Wait(500) {home} $prefix
#                         # can only select via first letter due to broken UI <<<>>>
#                       {alt+r} U {alt+n} {enter};

BookmarkThat(prefix)  := Beep();
BookmarkTitle(prefix) := Beep();


<bookmark> := ( work bookmark=W | personal bookmark=P | home bookmark=P 
              |      bookmark=IfHome(P,W) );
<thing>    := ( paper = PAPER | slides = SLIDES );


<bookmark>  that	      = BookmarkThat("[$1] ");
<bookmark> [that] as read     = BookmarkThat("[$1][R] ");

  # select title of <thing> before issuing these commands:
<bookmark> <thing>	      = BookmarkTitle("[$1] $2: ");
<bookmark> <thing> as read    = BookmarkTitle("[$1][R] $2: ");
<bookmark> <thing> as printed = BookmarkTitle("[$1][pri] $2: ");



##
## Commands for organizing bookmarks:
##

# <name of folder> while bookmarks have focus

# can sort bookmarks by name via right click menu of folder when one
# of its *items* is selected.


organize favorites = Favorites() Wait(500) o{enter}
		     WaitForWindow("Organize Favorites")
		        # make organizing window bigger:
		     {alt}{space}s {right_20} {down_10} {enter}
		     {alt}{space}s {up_10} Wait(200) {enter}
		        # select first bookmark:
		     {alt+r}{esc};

Organize Favorites:

    # move focus to list of bookmarks:
  focus [on] bookmark = {alt+r}{esc};
  
  # use alt+up/down to move items within a level
  
  #  should no longer need this functionality:
  #file <folder_name>  = {alt+m} {shift+tab_2} $1 {enter} {alt+r}{esc};
  
  please destory 1..10 (item|items)  = 
       Repeat($1, {alt+d} Wait(0) Y Wait(100)
       SwitchTo("^Organize Favorites$"));

:



## 
## Exporting bookmarks:
## 

ImportAndExport()      := File() Wait(100) m 
	                  WaitForWindow("Import/Export Settings") Wait(1000);

Export0()              := ImportAndExport() {alt+e}{enter} {space}{enter};

Export(selector, path) := Export0() Wait(300)
		          $selector Wait(1000)
		          {enter} $path Wait(100) {enter} y;


manually export bookmarks   = Export0();

#upload links                = Export(Upload, UNIX(work:~/http/bookmark.htm));
#upload links to (foil|work) = Export(Upload, UNIX(  $1:~/http/bookmark.htm));



## 
## Backing up bookmarks:
## 

  # this fails to save ordering of bookmarks (sorts them):
  #   (original order is stored in a registry key)
push bookmarks =
    PrepareUpload(@~/backups/bookmarks)
    Export("", UploadDir() /IE-bookmarks- Date.Now("%Y-%m-%d") .htm)
    Wait(2000) # <<<>>>
    DoUpload();

manually push bookmarks =
    Export("", PC(~/scratch/outgoing/IE-bookmarks- Date.Now("%Y-%m-%d") .htm));


CD_FOLDER(pathname) := AppBringUp("explorer@" $pathname, "$pathname");

  # do 'erase' then 'key Yankee' afterwards:
destroy all favorites = CD_FOLDER(PC(~/../Favorites)) {ctrl+a};

  # this adds bookmarks to favorites; remove other stuff first...
  # sadly, this does not preserve order...
  # also fails to restore file://'s and empty folders :-(
import bookmarks = ImportAndExport() {alt+i}{enter} {space}{enter}
	             UNIX(work:~/backups/bookmarks/IE-bookmarks-);

manually import bookmarks = ImportAndExport() {alt+i}{enter} {space}{enter};
