###
### Commands for Internet Explorer (version 11)
###
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
###
###   See also iexplore_bookmark.vcl
### 

include "locale_PC.vch";
include "iexplore.vch";


## 
## Window manipulation commands:
## 

System(key) := SendSystemKeys({alt+space}) Wait(100) $key;

  # to override global commands:
include "windows.vch";

  # DNS IE built-ins are broken; replace with working versions:
include "new_windows.vch";



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
