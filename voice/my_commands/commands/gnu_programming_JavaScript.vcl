###
### Voice commands for programming in JavaScript
###

include "gnu.vch";
include "letters.vch"; 
include "string.vch";

Place(before, after) := $before $after {left_ Len($after) };

End in script block = {end}{shift+space} '{' {enter} '});' {tab} {home}{ctrl+o}{tab};
