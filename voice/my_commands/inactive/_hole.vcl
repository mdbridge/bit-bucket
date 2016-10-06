### 
### Experimental implementation of a keystroke grammar using only
### string functions.
### 

include "string.vch";

<e> := (
         0 = @d0@R
       | 1 = @d1@R
       | 2 = @d2@R
       | 3 = @d3@R
       | 4 = @d4@R
       | 5 = @d5@R
       | 6 = @d6@R
       | 7 = @d7@R
       | 8 = @d8@R
       | 9 = @d9@R

       | digit = @D@o

       | Alpha = @Da@R
       | Bravo = @Db@R
       | Charlie = @Dc@R
       | Delta = @Dd@R
       | echo = @De@R
       | foxtrot = @Df@R
       | golf = @Dg@R
       | Hotel = @Dh@R
       | India = @Di@R
       | Juliett = @Dj@R
       | kilo = @Dk@R
       | Lima = @Dl@R
       | Mike = @Dm@R
       | November = @Dn@R
       | Oscar = @Do@R
       | Papa = @Dp@R
       | Quebec = @Dq@R
       | Romeo = @Dr@R
       | Sierra = @Ds@R
       | tango = @Dt@R
       | uniform = @Du@R
       | Victor = @Dv@R
       | whiskey = @Dw@R
       | xray = @Dx@R
       | Yankee = @Dy@R
       | Zulu = @Dz@R

       | up	   = @Dup@r
       | down      = @Ddown@r
       | left	   = @Dleft@r
       | right	   = @Dright@r
  
       | page up   = @DPgUp@r
       | page down = @DPgDn@r
  
       | home	   = @Dhome@r
       | end       = @Dend@r
  
       | back      = @Dbackspace@r
       | delete    = @DDel@r
  
       | space	   = @Dspace@r
       | tab	   = @Dtab@r
       | enter     = @Denter@r
       | escape    = @Desc@r


       | shift   = @Dshift+@o
       | alt     = @Dalt+@o
       | control = @Dctrl+@o
       );

Stroke(elements) := {
		    Replace(
		      Replace(
		        Replace(
			  Replace(
			    Replace(
  		              Replace(@o $elements @D@e, @r@d, " "),
	  	            @R, @r),
		          @D, @d),
		        @o@d, ""),
		      @r@d, "}{"),
		    {@e, "");
		     


hole <e> = Stroke($1);
hole <e> <e> = Stroke($1$2);
hole <e> <e> <e> = Stroke($1$2$3);
