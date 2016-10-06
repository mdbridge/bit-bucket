### 
### Voice commands for editing HTML:
### 
###   Very simple HTML macros for the subset of HTML that I actually use.
### 
###   See ../inactive/_any_HTML.vcl for more a comprehensive version.
### 

include "extended_string.vch";


## See programming_HTML.txt in vocabulary for common in-line tags



HTML pair = '<a href=""></a>{Left 6}'
            {shift+left}{shift+right}; # acts like no space for non-standard apps

  # spacing of next dictated phrase in Win32Pad sucks <<<>>> break into two?
insert URL = '<a href="' Clipboard.Get() '"></a>{Left 4}';


protect tag <_anything> = Replace($1, "<", "&lt;");









  # Input:  string $s containing at most one %, $s contains no braces
  # Output: types $s without any %'s except that it replaces @'s with
  #         enter's then moves cursor back to where % was if any was present
Position(s) := Replace2($s, "%", "", "@", {enter})
               '{left ' Len( Split("$s%", "%", 1) ) };



## 
## Inline tags:
## 

#<inline> := (
#    title                      |
#    body                       |
#    Heading 1         = h1     |
#    Heading 2         = h2     |
#    Heading 3         = h3     |
#    Heading 4         = h4     |
#    Heading 5         = h5     |
#    Heading 6         = h6     |
#    
#    Preformatted Text = pre    |
#    Emphasize         = em     |
#    Code              = code   |
#    
#    Bold              = b      |
#    Italic            = i      | Italics    = i  | 
#    
#    List Item         = li     |
#    menu                       |
#    Directory List    = dir    | Directory = dir | 
#    Definition List   = dl     |
#    Definition Term   = dt     |
#    Data Definition   = dd     |
#    
#    caption                    |
#    Table Row         = tr     |
#    Table Heading     = th     |
#    Table Detail      = td     |
#    col                        | Column    = col 
#);
#
#
#  # handle DNS 10.1 bug with HeardWord(\No-Space):
#TightCloseBend() := HeardWord(>\close-angle-bracket, \No-Space);
#
#             <inline> Tag      = <$1 TightCloseBend();
#
#(start|open) <inline>          = <$2 TightCloseBend();
#(end|close)  <inline>          = </$2>;
#
#             <inline> Tag pair = Position(<$1%</$1>) TightCloseBend();
