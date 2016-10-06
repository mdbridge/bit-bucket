#############################################################################
#                                                                           #
# Macros for the ElephantStore project                                      #
#                                                                           #
#############################################################################

include "gnu.vch";
include "java_types.vch";


elephant store directory	= "com/hp/elephantstore/";

#elephant store package		= "com.hp.elephantstore.";


#
# Inserting trace code:
#

make trace entry =
     	'{home}{ctrl+o}{ctrl+o}{tab}Trace.entry(this, "");{left 3}';

make trace exit =
	'{home}{ctrl+o}{ctrl+n}{ctrl+o}{tab}Trace.exit(this, "");{left 3}';

make trace info =
	'{home}{ctrl+o}{tab}Trace.info(this, "", "{left 4}';

                                                                            
#  load ElephantStore tags file:
load elephant tags =
	Do(visit-tags-table)
	{home}{ctrl+k} UNIX(work:~/deduplication/client/TAGS) {enter};



#  switch-to-buffer/find file for my given java type:
buffer <java_type> = 
        FindFile(UNIX("work:~/deduplication/client/src/$1.java") {enter});


<elephant_file> := (
       Makefile = ~/deduplication/client/Makefile
     | message type table = 
   ~/deduplication/client/src/com/hp/elephantstore/protocol/MessageType.table
);

buffer <elephant_file>  = FindFile(UNIX(work:$1) {enter});


include "string.vch";

<leap> type <java_type> = Leap($1, Split($2, "/", -1));
