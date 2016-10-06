###
### Commands for Internet Explorer (version 8)
###
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
###

include "string.vch";
include "switch.vch";
include "locale_PC.vch";

include "iexplore.vch";



## 
## Window manipulation commands:
## 

#
# IE7 & IE8 hang if they are given {alt+space} at the top level [DNS bug]
#
# However, {alt}{space} seems to work okay.  The right-click trick
# also works.
#
# Edit: no, {alt}{space} doesn't work for embedded PDF documents.
#       {f10}{space} does, however.
#
#System(key) := {f10}{space} Wait(100) $key Wait(100);
#System(key) := {f10_2}{space} Wait(100) $key Wait(100);
System(key) := SetMousePosition(1, 10, 10) ButtonClick(2, 1) Wait(100) 
               $key Wait(10);

include "windows.vch";

  # so work with PDF documents:
(close=C | minimize=n | minimise=n | maximize=x | restore=R) [the] window 
	= System($1);



##
## Going to Bookmarks:
##

include "bookmark_folders.vch";

# switch to favorites

<folder_pos> bookmarks = {esc_4} Favorites() o{down_$1} {right};



## 
## Creating bookmarks:
## 

  # add current URL to list of links to upload to email:
BookmarkThat(extra) := Address() {ctrl+d} 
		       WaitForWindow("Add a Favorite") 
		       Wait(500) $extra
                        # can only select via first letter due to broken UI <<<>>>
                      {alt+r} U {alt+n} {enter};

<context> := ( work=W | personal=P | home=P );

<context> bookmark  that          = BookmarkThat("{home}[$1] ");
<context> bookmark [that] as read = BookmarkThat("{home}[$1][R] ");

bookmark  that          = Beep();
#bookmark  that          = BookmarkThat("");
#bookmark [that] as read = BookmarkThat({home} "[R] ");



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
		     {alt}{space}s {up_10} {enter}
		        # select first bookmark:
		     {alt+r}{esc};

  # not sure this is working reliably in IE 8:
Organize Favorites:

  # move focus to list of bookmarks:
focus [on] bookmark = {alt+r}{esc};


# use alt+up/down to move items within a level

file <folder_name>  = {alt+m} {shift+tab_2} $1 {enter} {alt+r}{esc};


please destory 1..10 (item|items)  = 
     Repeat($1, {alt+d} Wait(0) Y Wait(100)
     SwitchTo("Organize Favorites"));

:



## 
## Exporting bookmarks:
## 

Export0()              := File() Wait(100) i i {enter}
	                  WaitForWindow("Import/Export Settings") Wait(100)
			  {alt+e}{enter} {space}{enter};

Export(selector, path) := Export0() Wait(300)
		          $selector Wait(1000)
		          {enter} $path Wait(100) {enter} y;


  # manual choices:
export bookmarks        =  Export0();


  # <<<>>>
upload links                = Export(Upload, UNIX(work:~/http/bookmark.htm));
upload links to (foil|work) = Export(Upload, UNIX(  $1:~/http/bookmark.htm));


<save> := ( web  = "Web,"  UNIX(work:~/voice/my_commands/web.htm)
          | work = "Work," UNIX(work:~/voice/my_commands/work.htm) );

export <save> bookmarks = Export(Split($1,",",0), Split($1,",",1));


<location> := ( home | work );

backup <location> bookmarks = Export("", UNIX(work:~/backups/bookmarks/ $1.htm));



## 
## Importing bookmarks:
## 

CD(pathname)     := AppBringUp("explorer@" $pathname,
		               "$pathname");

  # do 'erase' then 'key Yankee' afterwards:
destroy all favorites = CD(PC(~/../Favorites)) {ctrl+a};


  # this adds bookmarks to favorites; remove other stuff first...
  # sadly, this does not preserve order...
import bookmarks = File() Wait(100) i i {enter}
	                  WaitForWindow("Import/Export Settings") Wait(300)
			  {enter}
			  {space}{enter}
			  UNIX(work:~/backups/bookmarks/work.htm);



## 
## Navigation:
## 

# 
# Within a page:
# 

# {next|previous} {frame|pane}

# click <words in a link>

# click {text link, button, checkbox, edit box, image, list box, radio
# 	 button, text field, type text}


# 
# Within history of pages:
# 

# go (back|forward) <n> times



# 
# Across tabs:
# 

# view <n>th tab
# view {first,last,next,previous} tab

# open quick tabs

# close tab, close other tabs



##
## Internet security settings:
##

  #  go to security settings for Internet zone; not with PDF documents:
(open|Internet) security options = 
	Tools()o WaitForWindow("Internet Options", "", 1000)
	{ctrl+tab} i {alt+c};



##
## Miscellaneous:
##

copy that = {ctrl+c};

# open privacy report

pane (up={PgUp}|down={PgDn}) = HeardWord(next, pane) $1;

# full screen [off]

  # parallel with Firefox:
(show|view) [page] source = Address() {alt+v} Wait(100) c;

