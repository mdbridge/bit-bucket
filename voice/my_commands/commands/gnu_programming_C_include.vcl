###
### Voice commands for adding includes to C and C++ programs
###


##
## By "need":
##

<need> := (
            standard out	= '#include <stdio.h>'
          | standard error	= '#include <stdio.h>'
          | print F		= '#include <stdio.h>'
          | exit		= '#include <stdlib.h>'
          | memory set		= '#include <string.h>'
          | memory copy		= '#include <string.h>'
          | string copy		= '#include <string.h>'
          | null		= '#include <stddef.h>'
          | assert		= '#include <assert.h>'
          | size_t		= '#include <stddef.h>' # also stdlib.h
          | int 64 T		= '#include <stdint.h>'
          | fixed size integers = '#include <stdint.h>'
          | errno		= '#include <errno.h>'
          | string errno	= '#include <string.h>' # strerror*
          | open		= '#include <sys/types.h>{enter}'
                                  '#include <sys/stat.h>{enter}'
                                  '#include <fcntl.h>'
          | M map		= '#include <sys/mman.h>'
          | close		= '#include <unistd.h>'
          | unlink		= '#include <unistd.h>'

	    # bold:
          | fabric atomics    = '#include <fam_atomic.h>'

	  # C++ includes
	  | plus strings      = '#include <string>'
	  | plus string	      = '#include <string>'
	  | [plus] functions  = '#include <functional>'
	  | [plus] exceptions = '#include <stdexcept>'
	  | C out	      = '#include <iostream>'
	  | C err	      = '#include <iostream>'
	  | type ID	      = '#include <typeinfo>'
	  | unordered map     = '#include <unordered_map>'
	  | type traits       = '#include <type_traits>'
	  | vectors           = '#include <vector>'
	  | algorithms        = '#include <algorithm>'
);

add includes for <need> = $1{enter};



##
## By library name:
##

<library> := ( 
                 # standard C header files:
               assert		    = <assert.h>
             | complex		    = <complex.h>
             | C type		    = <ctype.h>
             | error no		    = <errno.h>
             | F environment	    = <fenv.h>
             | float		    = <float.h>
             | int types	    = <inttypes.h>
             | ISO 646		    = <iso646.h>
             | limits		    = <limits.h>
             | locale		    = <locale.h>
             | math		    = <math.h>
             | set jump		    = <setjmp.h>
             | signal		    = <signal.h>
             | standard align	    = <stdalign.h>
             | standard arg	    = <stdarg.h>
             | standard atomic	    = <stdatomic.h>
             | standard bool	    = <stdbool.h>
             | standard definitions = <stddef.h>
             | standard int	    = <stdint.h>
             | standard I/O	    = <stdio.h>
             | standard lib	    = <stdlib.h>
             | standard library	    = <stdlib.h>
             | standard no return   = <stdnoreturn.h>
             | string		    = <string.h>
             | TG math		    = <tgmath.h>
             | threads		    = <threads.h>
             | time		    = <time.h>
             | U char		    = <uchar.h>
             | W char		    = <wchar.h>
             | WC type		    = <wctype.h>
                 # UNIX header files for usual system calls:
             | directory entry	    = <dirent.h>
             | F control	    = <fcntl.h>
             | file		    = <sys/file.h>
             | M man		    = <sys/mman>
             | socket		    = <sys/socket>
             | stat		    = <sys/stat>
             | types		    = <sys/types>
             | Unix standard	    = <unistd.h>
	        # other C headers:
             | P threads            = <pthread.h>
	        # other C++ headers:
	     | I/O stream	    = <iostream> 
             );

include <library> = "#include $1" {enter};
