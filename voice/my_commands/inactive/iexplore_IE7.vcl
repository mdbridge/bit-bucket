###
### Commands for Internet Explorer (version 7)
###
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
###

include "string.vch";

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
System(key) := {f10}{space} Wait(100) $key Wait(100);

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

  # add current URL to list of Links to upload to email:
bookmark that = Address() {ctrl+d} 
	        WaitForWindow("Add a Favorite") 
                  # can only select via first letter due to broken UI <<<>>>
                {alt+r} L {alt+n} {enter};



##
## Commands for organizing bookmarks:
##

# <name of folder> while bookmarks have focus

organize favorites = Favorites() o{enter}
		     WaitForWindow("Organize Favorites")
		        # make organizing window bigger:
		     {alt}{space}s {right_20} {down_10} {enter}
		     {alt}{space}s {up_10} {enter}
		        # select first bookmark:
		     {alt+r}{esc};

Organize Favorites:

  # move focus to list of bookmarks:
focus [on] bookmark = {alt+r}{esc};


file <folder_name>  = {alt+m} {shift+tab_2} $1 {enter} {alt+r}{esc};


delete 1..10 items  = Repeat($1, {alt+d} Wait(0) Y);

:



## 
## Exporting bookmarks:
## 

Export0()              := File() Wait(100) i
	                  WaitForWindow("Import/Export Wizard")
                          n {down}{enter};

Export(selector, path) := Export0()
		          $selector Wait(1000)
		          {enter} {tab} $path {enter} y;

  # manual choices:
export bookmarks        =  Export0();


upload links = Export(Links, z:\http\bookmark.htm);


<save> := ( web  = "Web,z:\voice\my_commands\web.htm" 
          | work = "Work,z:\voice\my_commands\work.htm"    );

export <save> bookmarks = Export(Split($1,",",0), Split($1,",",1));


<location> := ( home | work );

backup <location> bookmarks = Export("", z:\backups\bookmarks\ $1.htm);



## 
## Navigation:
## 

# 
# Within a page:
# 

# {next|previous} {frame|pane}

image 1..20 = HeardWord(image) HeardWord(choose, $1);


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

  #  this needs to be updated...  <<<>>>
SetActiveX(setting) := 
	Tools()o {ctrl+tab} i {alt+c}
	{left}{right}
	Replace(
	Replace(
	Replace(
	Replace(
	Replace(
	Replace(
	   ". .DE.P. ..D.EP. .D.E.P. .{space}... .{space}... ..D.E.P. .D.E.P.",
	   " ", ""),
	   $setting, {space}),
	   ".", {down}),
	   "D", ""),
	   "E", ""),
	   "P", "")
	{enter} y {enter};

#ActiveX on     = SetActiveX(E);
#ActiveX off    = SetActiveX(D);
#ActiveX prompt = SetActiveX(P);



##
## Miscellaneous:
##

copy that = {ctrl+c};

# open privacy report

pane (up={PgUp}|down={PgDn}) = HeardWord(next, pane) $1;
