### 
### Voice commands for Firefox (version 3.5)
### 
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
### 

include "locale_PC.vch";
include "Firefox.vch";

FixFocus() := Address() UnAddress();



## 
## Window manipulation commands:
## 

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

# {view, hide} bookmarks sidebar

<folder_pos> bookmarks = {esc_4} Address() {alt+b} Wait(100) 
                         {down_7} {down_$1} {right};



## 
## Creating bookmarks:
## 

  # add current URL to list of Links to upload to email:
BookmarkThat(extra) := Address() {ctrl+d} Wait(100) $extra
                       {home} {tab} Links {alt+d} FixFocus();

bookmark  that          = BookmarkThat("");
bookmark [that] as read = BookmarkThat({home} "[R] ");



##
## Commands for organizing bookmarks:
##

organize (favorites|bookmarks) = Address() {ctrl+shift+b};

Library:
file <folder_name> = {alt+o} Wait(100) m Wait(100) $1 {enter};
:



## 
## Exporting bookmarks:
## 

Library() := Address() {ctrl+shift+b} WaitForWindow(Library) Wait(100);

ImportAndBackup(letter) := Library() {alt+i} Wait(100) $letter;


Export0()    := ImportAndBackup(e) WaitForWindow("Export Bookmarks File");

  # this is more fragile than I would like...  <<<>>>
Export(path) := Export0() Wait(1000) $path Wait(100) {enter};


  # manual choices:
export bookmarks = Export0();

upload links     = Export(UNIX(~/http/Firefox-bookmarks.htm));


<location> := ( home | work );

backup <location> bookmarks = Export(UNIX(~/backups/bookmarks/Firefox-$1.htm));



## 
## Moving the exact set of bookmarks (including menu bar) between two
## instances of Firefox:
## 

push bookmarks = 
    ImportAndBackup(b) WaitForWindow("Bookmarks backup filename")
    Wait(100) {ctrl+c} UNIX(~/backups/bookmarks/) {ctrl+v};

pull bookmarks =
    ImportAndBackup(rc) WaitForWindow("Select a bookmarks backup") 
    Wait(100) UNIX(~/backups/bookmarks/) Wait(100) {enter};



## 
## Navigation:
## 

# 
# Within a page:
# 

next     frame = {f6};
previous frame = {shift+f6};



## 
## Clicking links/gizmos:
## 

# 
# These commands require the mouseless browsing extension.
# 
# Plug-in configuration:
# 
#   ID-types->modifier for enabling IDs...    = Alt (others ctrl+Alt)
#   Keys->Postfix key to open link in new tab = MULTIPLY (press num pad star)
#   Keys->blur active element                 = ctrl+DIVIDE 
#                                                  (press control num pad slash)
#
# Also, remove Dragon's use of MULTIPLY and DIVIDE
# 

Blur()   := {ctrl+NumKey/};
Toggle() := {NumKey.};

<pick> := (        pick = {enter}   |       new pick = {NumKey*} 
          | window pick = {NumKey/} | selecting pick = {shift}   );

<pick> 0..9           = {alt+$2}                 $1;
<pick> 0..9 0..9      = {alt+$2}{alt+$3}         $1;
<pick> 0..9 0..9 0..9 = {alt+$2}{alt+$3}{alt+$4} $1;


show    numbers = Blur() Toggle();
refresh numbers = Blur() Toggle() Toggle();


# 
# These commands use the built-in quick find link:
# 

       link <_anything> = Blur() "'$1" {enter};
new    link <_anything> = Blur() "'$1" {ctrl+enter};
window link <_anything> = Blur() "'$1" {shift+enter};



##
## Miscellaneous:
##

# {increase, decrease} font

# view source

pane (up={PgUp}|down={PgDn}) = FixFocus() $1;

force refresh = {ctrl+shift+r};



## 
## Experimental:
## 

include "string.vch";

label <_anything> = Blur() Replace("/$1", " :", ":") Wait(100) {tab};
