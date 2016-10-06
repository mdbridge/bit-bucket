###
### Commands for navigating within Gnuemacs buffers to a single position.
### Some commands optionally erase/cut as go.
###
### Other movement commands can be found in:
###
###   gnu_short.vcl
###   gnu_locate.vcl (generalized search)
###

include "gnu.vch";
include "numbers.vch";
include "letters.vch";
include "printables.vch";
include "unpack.vch";
include "optional.vch";



### 
### Compass commands: commands for moving the cursor in a given direction,
### 	    	      optionally erasing/cutting as go.
### 
### See also compass commands in gnu_short.vcl.
### Many commands override commands in _any_compass.vcl.
### 

## 
## Overridden versions of _any_compass.vcl commands:
## 

# 
# Commands for moving to the extreme of a compass direction:
# 

<long_extreme> := (
       top-of-file     = {ctrl+home}
     | top-of-buffer   = {ctrl+home}

     | end-of-file     = {ctrl+end}
     | end-of-buffer   = {ctrl+end}
);

<long_extreme> = $1 Empty();


## 
## Commands for moving a fixed distance in a compass direction:
## 

<direction> := (
        # by sentences:
       flee  sentence  = {esc}a
     | jump  sentence  = {esc}e
     | kill  sentence  = "{esc}a,{ctrl+w}," Mark()
     | toast sentence  = "{esc}e,{ctrl+w}," Mark()

        # by paragraphs:
     | flee  paragraph = "{esc}{{}"
     | jump  paragraph = "{esc}}"
     | kill  paragraph = "{esc}{{},{ctrl+w}," Mark()
     | toast paragraph = "{esc}},{ctrl+w}," Mark()

        # by Emacs groups:
     | flee  group     = {esc}{ctrl+b}
     | jump  group     = {esc}{ctrl+f}
     | kill  group     = "{esc}{ctrl+b},{ctrl+w}," Mark()
     | toast group     = "{esc}{ctrl+f},{ctrl+w}," Mark()
);

<direction> [<my1to100>] = Unpack3($1, When($2,$2,1));


## 
## Leap commands:
## 

include "leap4_long.vch";

# 
# Target: user supplied string (prompted for via the mini-buffer):
# 
leap    = Do(nonincremental-search-forward);
retreat = Do(nonincremental-search-backward);

# 
# Target: clipboard
# 

  # note: putting clipboard into current isearch string isn't portable
  #       (keys required depends on the Emacs version)
<leap> clipboard = Leap($1, Clipboard.Get());

# 
# Target: the first (Emacs) word starting with the given character(s):
# 
<leap> word <prn> [<prn>] = LeapRegexRaw4($1, \b$2$3\w*, 1, m);
<kill> word <prn> [<prn>] = LeapRegexRaw4($1, \b$2$3\w*, 1, x);


## 
## Emacs-specific compass characters:
## 

hard tab [2..10]       = When($1,{ctrl+u}$1) {ctrl+q}{tab};

line feed [<my1to100>] = REPEAT($1, {ctrl+j});


## 
## Special combinations:
## 

destroy rest [destroy] next [<my1to100>] =
	Mark() {down When($1,_$1)} {end} {ctrl+w} Exchange();



### 
### Move to a specific word/character on the current screen:
### 

Ace(regex)    := Elisp('(mdl-Ace-jump "$regex")');

  # adjust to ignore hyphenation, lose of spaces, line wrap, underscores:
Adjust(words) := Replace($words, " ", "[-_]?\\s-*");

word	   <letter>    = Mark() SavePoint(*)         {ctrl+c}{space} $1;
character  <prn>       = Mark() SavePoint(*) {ctrl+u}{ctrl+c}{space} $1;

anchor	   <_anything> = Mark() SavePoint(*) Ace(\\< Adjust($1));
mid anchor <_anything> = Mark() SavePoint(*) Ace(    Adjust($1));

visible    <_anything> = Mark() SavePoint(*) Ace(\\< Symbols.Monster($1) \\>);



### 
### Movement not based on the (exact) current point location:
### 

##
## Jump to start of absolute line number L:
##

  # go to absolute line number $line:
GotoLine(line) := Do2(goto-line, $line);

GotoLine3(thousands, hundreds, ones) := 
    GotoLine(Eval($thousands*1000 + $hundreds*100 + $ones));

  # prompt user for L:
real line = Do(goto-line);

real line                       <my0to99>  = GotoLine($1);
real line    <my0to99> hundred [<my0to99>] = GotoLine3( 0,$1,When($2,$2,0));
real line <my0to99> thousand [<my0to99> hundred] [<my0to99>] =
     GotoLine3($1,When($2,$2,0),When($3,$3,0));


# <<<>>>
<d> := 0..9;

force go 		       = Do(goto-line);    # prompt user for L
force go <d> [<d> [<d> [<d>]]] = Do(goto-line) $1$2$3$4 {enter};



##
## Jump to a previously saved location even if it's in another buffer
##

  # save a marker to point so we can return to it later:
drop crumb = SavePoint(0);

  # set mark then jump to previously dropped crumb:
find crumb = Mark() RestorePoint(0);


drop crumb <letter> = SavePoint($1);
     crumb <letter> = Mark() RestorePoint($1);
