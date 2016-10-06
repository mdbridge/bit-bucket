###
### Commands for Internet Explorer (version 6)
###
###     This file contains website independent commands; see
###   iexplore_sites.vcl for website specific commands.
###

include "string.vch";



##
## Going to Bookmarks:
##

OpenURL(URL) := {ctrl+o} $URL {enter};
#OpenURL(URL) := SendSystemKeys({ctrl+o} $URL {enter});
#OpenURL(URL) := {alt+d} $URL {enter};

  #  go to URL stored in the clipboard:
import URL       = OpenURL({ctrl+v});


include "websites.vch";

<folder> := ( Web = 1   | Work = 2     | Comics = 3 | Feeds = 4 
	    | Other = 5 | Software = 6 | Tools = 7  | Voice = 8 
	    | Links = 9 );

<folder> bookmarks = {esc_4} {alt+a}{down}{down_$1} {right};


Web  <web_site>  = OpenURL($1);
Work <work_site> = OpenURL($1);



## 
## Creating bookmarks:
## 

  # add current URL to list of Links to upload to email:
bookmark that = {alt+a}{enter} {alt+i} Links {alt+n} {enter};



##
## Commands for organizing favorites:
##

organize favorites = {esc} {alt+a}o{enter}
		        # make organizing window bigger:
		     {alt+space}s {right_20} {down_20} {enter}
		     {alt+space}s {up_20} {enter}
		        # select first bookmark:
		     {alt+r}{esc};

  # move focus to list of bookmarks:
focus right = {alt+r}{esc};


<bookmark_folder> := ( Favorites | Web | Work | Voice | Links );

file <bookmark_folder> = {alt+m} $1 {enter} {alt+r}{esc};


#Dragon: {next|previous} frame

delete 1..10 items = Repeat($1, {alt+d} Wait(0) Y);



##
## Internet security settings:
##

open security options = {alt+t}o {ctrl+tab} i {alt+c};

SetActiveX(setting) := 
	{alt+t}o {ctrl+tab} i {alt+c}
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

ActiveX on     = SetActiveX(E);
ActiveX off    = SetActiveX(D);
ActiveX prompt = SetActiveX(P);

  #  go to security settings for Internet zone:
Internet security settings =
	{alt+t}O{ctrl+tab}{tab}C;



##
## Miscellaneous:
##

  #  go to top of page then refresh:
refresh top = {ctrl+home}{ctrl+r};

go back 1..10 = Repeat($1, SendSystemKeys({alt+left));

  #  a better version of go back:
really go back       = SendSystemKeys({alt+left});
really go back 1..10 = SendSystemKeys({alt+left_$1});

image 1..20 = HeardWord(image) HeardWord(choose, $1);

copy address = {alt+d}{ctrl+c}{tab};

copy that = {ctrl+c};



### 
### Filetype-specific commands:
### 

pdf:

go back = {alt+o}b;

:
