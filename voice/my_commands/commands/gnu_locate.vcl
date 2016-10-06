#############################################################################
#                                                                           #
# Voice commands for locating all occurrences of a text pattern             #
#                                                                           #
#############################################################################

include "letters.vch";

include "gnu.vch";
include "extended_string.vch";

include "projects.vch";



##
## Basic incremental search  (see gnu_movement.vcl for leap commands) 
##

  # start interactive search:
#start searching           = {ctrl+s};     # going forward
#start searching backwards = {ctrl+r};     # going backwards

  # start regex incremental search going forwards:
regex search   = {ctrl+u}1{ctrl+s};

  # repeat previous incremental search for same string going forwards
#search again   = {ctrl+s}{ctrl+s};

  # useful for constructing complicated regular expressions:
regexp builder = Do(re-builder);



## 
## Teleport commands (find all occurrences in a file):
## 

Format(text) := Replace2($text, "_ ", "_", " _", "_");

teleport                     = Do(mdl-occur);
teleport clipboard           = Occurs({ctrl+y});
teleport phrase  <_anything> = Occurs(Format($1));

teleport routine <_anything> = Occurs("^ *\(def\|sub\|public\|(defun\).*\(_\|\b\)" 
                                      Lower(Replace(Format($1)," ","[-_]?")));

teleport monster <_anything> = Occurs(\< Symbols.Monster($1)\>);

teleport class			    = Occurs("^ *\(class\|struct\)");
teleport (public|protected|private) = Occurs("^ *$1");

teleport non-ASCII		    = Occurs('[^{ctrl+q}{ctrl+a}-~]');


  # after teleport, go to occurrence displayed on line N:
teleport <r> =
	{ctrl+x}b *Occur* {enter}
	LineMod($1)
	{enter} {ctrl+x}1 {ctrl+x}b {enter} Do(bury-buffer);

  # assumes other window is the occurs buffer:
try teleport <r> = {ctrl+x}o LineMod($1) {enter};


  # experimental: <<<>>> do not use with teleport #; use with etags,
  # must start from a file covered by the current tags table:
teleport tag = {ctrl+u} Do(mdl-copy-buffer-filename) 
	       Do(list-tags) {ctrl+y}{enter}  {ctrl+x}o {ctrl+x}1;



## 
## Highlighting all occurrences of a regex in a file:
## 

Highlight(regexp) := Do2(highlight-regexp, $regexp);

highlight regexp    = Do(highlight-regexp);
highlight non-ASCII = Highlight('[^!-~ {ctrl+q}{ctrl+j}{ctrl+q}{ctrl+i}]');



## 
## Searching across files:
## 

  # do a regexp search on all files in tags table, jump to first match:
tags search        = Do(tags-search);
tags next [match]  = "{esc},";


  # do a regexp search on all marked files in dired mode, jump to first match:
Mondo search       = A;
Mondo search word  = A \<\> {left_2} Empty();
Mondo next [match] = "{esc},";


<grep> := ( grep=mdl-lgrep | grep recursively=mdl-rgrep );

  # prompts for file pattern, uses current directory:
run <grep>                = Do($1);
run <grep> [on] clipboard = Do($1) {ctrl+y}{enter};

  # similar but allow specifying options, no default file pattern:
grep             command = Do(grep)  "'' *" {left_3};
grep recursively command = Do(grep-find);

(next=""|previous={ctrl+u}-1) match = $1{ctrl+x}`;  # also works for occurs buffers



## 
## Searching across predefined sets of files:
## 

locate tag                    = Do (tags-apropos);
locate tag clipboard          = Do2(tags-apropos, {ctrl+y}) {ctrl+x}o {ctrl+x}1;
locate tag phrase <_anything> = Do2(tags-apropos, $1)       {ctrl+x}o {ctrl+x}1;


#
# Non-recursively for a target directory:
#

Locate(directory, which, pattern) :=
    Do(lgrep) $pattern{enter} $which{enter} UNIX($directory) {enter};

FinishLocate() := {ctrl+x}1 {ctrl+x}b "*grep*" {enter};

Locate2(kind, pattern) := Locate(Split($kind,",",0), Split($kind,",",1), $pattern);


<kind> := ( command    = "~/voice/my_commands/commands"
                         ",*.vc[hl] *.off ../mini-commands/*.*[vh]"
          | vocabulary = "~/voice/my_vocabulary/vocabulary,*.txt symbols.in"

	  | elisp      = "~/elisp,*.el"
          | book       = "foil:~/wanted/books/,*.txt out"

          | email      = "~/voice/my_commands/commands/,"
                         "personal_emails.vch work_emails.vch"
          | HP email   = "~/Rmail/h,*[^~] x/*"

            # experiment; list of files not updated...<<<>>>
          | file       = "~/Tmp/,files"
# cd ~; find . -print | grep -v '~' | sed 's/^/ /' > ~/Tmp/files
          );


locate <kind>                    = {ctrl+u} Locate2($1, target)
	Leap(u, target) {Del_6} '""' {left}; # Empty();

locate <kind> phrase <_anything> = Locate2($1, $2)       FinishLocate();
locate <kind> clipboard          = Locate2($1, {ctrl+y}) FinishLocate();


#
# Recursively using find and advanced grep:
#

  # grep_args:
  #   -w for word search (exactly matches a \w+), -E for egrep (allows +, etc.)
Search4(root, find_args, grep_args, pattern) := 
    Do(grep-find) @    
    Leap(u, -e)    $grep_args " "
    Leap(u, -exec) $find_args " "
    {home} Leap(d, .) {Del} $root
    Leap(d, @){Del} "''" {left} $pattern;

Search3(where, grep_args, pattern) :=
    Search4(Split($where, ",", 0), Split("$where,", ",", 1), $grep_args, $pattern);

locate <where>                    = Search3($1, "", "");
locate <where> phrase <_anything> = Search3($1, "", $2)       {enter};
locate <where> clipboard          = Search3($1, "", {ctrl+y}) {enter};

  # the below cannot be recognized:<<<>>>
locate <where> word               = Search3($1, -w, "");
#       <where> word               = Search3($1, -w, "");


locate <where> class              = Search3($1, -E, "class +\> *($|[^;])") 
			    	    Leap(u,\);
locate <where> define             = Search3($1, "", "define.*");
locate <where> include            = Search3($1, -E, '["<].h[>"]') Leap(u,.);
locate <where> typedef            = Search3($1, "", "typedef.*\< *;" {left_3});

  # hopefully all but typedef:
locate <where> interface =
     Search3($1, -E, "(class|enum|struct|define) +\> *($|[^;])") Leap(u,\);



## 
## Looking up personal address information:
## 

include "email_addresses.vch";

FindRolodex() := FindFile(UNIX(~/mail/contacts/addresses.rol)) {enter};

LookupAddress(prefix) := FindRolodex() Occurs("^" $prefix ".*:");
LookupEmail(email)    := FindRolodex() Occurs($email);
	
show contacts			  = LookupAddress("[a-zA-Z]");
show contacts <letter> [<letter>] = LookupAddress($1$2);
show contacts phrase <_anything>  = LookupAddress(.*$1);

show contacts for      <personal_email> = LookupEmail($1);
show contacts for work <work_email>	= LookupEmail($1);
