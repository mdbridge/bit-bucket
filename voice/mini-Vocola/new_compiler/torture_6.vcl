### 
### These files comprise a torture test for any Vocola lexer
### 


## 
## Test when we are in or not in a context statement:
## 

i n: ;
i n:	;
i n:
;

i n:#
;
i n:"" ;
i n:'' ;

i n:( ;
i n:) ;
i n:[ ;
i n:] ;
i n:| ;
i n:, ; 
i n:; ;

i n:: ;
i n::= ;
i n::( ;

not in:::x ;


i n ";" ';' #;

: ;

not in:= ;
not in # : 
;

not in ; : ;

i n: