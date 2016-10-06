### 
### Voice commands for spelling things
### 

#include letters.vch;
include phonetic.vch;
include "string.vch";


# 
# Via commands with DNS 11.5-12.5 can use:
# 
#   spell ([Cap] a)...
#   can "all caps" that afterwards...
#
# Note that in DNS 11.5-12.5, spell won't work with Firefox, Windows
# Explorer renaming, or the start menu.
#

  # in _any_chord.vcl:
#big <letter> = Upper($1);


<op> := ( scribe = %s | giant = '%s.upper()' | big = '%s.capitalize()');

<op> <short> [<short> [<short> [<short> [<short> [<short> [<short> [<short> [<short> [<short>]]]]]]]]] = EvalTemplate($1,$2$3$4$5$6$7$8$9$10$11);


##
## Experiment: <<<>>>
##

marvel [<short>] [<short>] [<short>] <_anything> [<short>] [<short>] [<short>] = $1$2$3$4$5$6$7;
