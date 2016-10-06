### 
### Common syntax errors made when writing Vocola 2 programs, part 2.
### 


## 
## Unterminated quotations:
## 

common command = keystrokes "these have a return
in them";
another = keystrokes and more keystrokes;
resynchronize = here;

common command = keystrokes "these have a return
and another line of text.
in them";
another = keystrokes and more keystrokes;
resynchronize = here;

this can't work = actions;
resynchronize = here;


I forgot a closing quote = keystrokes "these have a return ;
another = keystrokes and more keystrokes;
resynchronize = here;


I forgot a opening quote = keystrokes these have a return" ;
another = keystrokes and more keystrokes;
resynchronize = here;


## 
## Forgot a "|":
## 

<lis1> := ( blue  = 1 |
            red   = 2 # |
            green = 3
          );
resynchronize = here;


<lis2> :=  blue  = 1 |
           red   = 2 # |
           green = 3
          ;
resynchronize = here;


	  
## 
## Extra "|"'s by mistake:
## 

<lis3> := ( 
       	  | blue  = 1 # extra bar on this line...
          | red   = 2 
          | green = 3
          );
resynchronize = here;

<lis4> := ( 
       	    blue  = 1
          | red   = 2 
          | green = 3
	  |  # extra bar on this line...
          );


<lis5> :=  
       	  | blue  = 1 # extra bar on this line...
          | red   = 2 
          | green = 3
          ;
resynchronize = here;

<lis4> :=  
       	    blue  = 1
          | red   = 2 
          | green = 3
	  |  # extra bar on this line...
          ;
resynchronize = here;



## 
## Empty lists:
## 

<menu> := ();
another = keystrokes and more keystrokes;
resynchronize = here;

<menu> := ;
another = keystrokes and more keystrokes;
resynchronize = here;



## 
## Lost end of a list:
## 

<lis3> := ( 
       	    blue  = 1 
          | red   = 2 
	  # lines lost here...
another = keystrokes and more keystrokes;
resynchronize = here;

<lis3> :=  
       	    blue  = 1 
          | red   = 2 
	  # lines lost here...
another = keystrokes and more keystrokes;
resynchronize = here;


<lis3> := ( 
       	    blue  = 1 
          | red   
	  # lines lost here...
another = keystrokes and more keystrokes;
resynchronize = here;

  # next UNDETECTABLE (valid code):
<lis3> :=  
       	    blue  = 1 
          | red   
	  # lines lost here...
another = keystrokes and more keystrokes;
resynchronize = here;



## 
## Lost start of a list:
## 

          | red   = 2 
          | green = 3
          );
another = keystrokes and more keystrokes;
resynchronize = here;

          | red   = 2 
          | green = 3
          ;
another = keystrokes and more keystrokes;
resynchronize = here;



## 
## 
## 

crap
