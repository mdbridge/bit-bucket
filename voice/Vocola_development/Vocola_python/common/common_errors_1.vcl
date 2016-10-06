### 
### Common syntax errors made when writing Vocola 2 programs, part 1.
### 


##
## Forgotten semicolons...
##

common command = keystrokes #;
another = keystrokes and more keystrokes;
resynchronize = here;

common command = keystrokes #;
another (red | blue) = keystrokes and more keystrokes;
resynchronize = here;

common command = keystrokes #;
another [optional] = keystrokes and more keystrokes;
resynchronize = here;


foo1(argument) := keystrokes #;
another = keystrokes and more keystrokes;  
resynchronize = here;

foo2(argument) := keystrokes #;
another (red | blue) = keystrokes and more keystrokes;
resynchronize = here;

foo3(argument) := keystrokes #;
another [optional] = keystrokes and more keystrokes;
resynchronize = here;


  # next UNDETECTABLE (valid code):
<list> := red | blue #;
another = keystrokes and more keystrokes;
resynchronize = here;

<list> := red | blue #;
another (red | blue) = keystrokes and more keystrokes;
resynchronize = here;

<list> := red | blue #;
another [optional] = keystrokes and more keystrokes;
resynchronize = here;


<list> := (red | blue) #;
another = keystrokes and more keystrokes;
resynchronize = here;

<list> := (red | blue) #;
another (red | blue) = keystrokes and more keystrokes;
resynchronize = here;

<list> := (red | blue) #;
another [optional] = keystrokes and more keystrokes;
resynchronize = here;  resynchronize = here;


  # next UNDETECTABLE (valid code):
include "Fred.vch" #;
another = keystrokes and more keystrokes;
resynchronize = here;

  # next UNDETECTABLE (valid code):
include "Fred.vch" #;
another (red | blue) = keystrokes and more keystrokes;
resynchronize = here;

  # next UNDETECTABLE (valid code):
include "Fred.vch" #;
another [optional] = keystrokes and more keystrokes;
resynchronize = here;  resynchronize = here;


  # next UNDETECTABLE (valid code):
$set variable value #;
another = keystrokes and more keystrokes;
resynchronize = here;

  # next UNDETECTABLE (valid code):
$set variable value #;
another (red | blue) = keystrokes and more keystrokes;
resynchronize = here;

  # next UNDETECTABLE (valid code):
$set variable value #;
another [optional] = keystrokes and more keystrokes;
resynchronize = here;  resynchronize = here;


common command = keystrokes #;
this is a context:
resynchronize = here;

foo4(argument) := keystrokes #;
this is a context:
resynchronize = here;

<list> := red | blue #;
this is a context:
resynchronize = here;

<list> := (red | blue) #;
this is a context:
resynchronize = here;

  # next UNDETECTABLE (valid code):
include "Fred.vch" #;
this is a context:
resynchronize = here;

  # next UNDETECTABLE (valid code):
$set variable value #;
this is a context:
resynchronize = here;



Forgot a semicolon = at the end of the file
