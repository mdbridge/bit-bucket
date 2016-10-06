###
### Lookup Java documentation family of commands
###

include "string.vch";

include "URLs.vch";


<Java_package> := (
       "java dot I/O"                      = "java dot io"
     | "java dot lang"
     | "java dot math"
     | "java dot network I/O dot channels" = "java dot nio dot channels"
     | "java dot network I/O"              = "java dot nio"
     | "java dot text"
     | "java dot util"
);

<Java_type> := (
       "java dot I/O dot File"             = "java dot io dot File"
     | "java dot lang dot String"
     | "java dot lang dot System"
     | "java dot lang dot Thread"
     | "java dot util dot Collection"
     | "java dot util dot Vector"
     | "java dot util dot List"
);


Base() := "http://download.oracle.com/javase/7/docs/";


lookup Java = Lookup(Base() "api/overview-summary.html");

lookup <Java_package> = 
	Lookup(Base() "api/" 
	          Replace($1," dot ","/") 
		  "/package-summary.html");

lookup <Java_type> =
	Lookup(Base() "api/" 
	          Replace($1," dot ","/") 
	 	  ".html");
