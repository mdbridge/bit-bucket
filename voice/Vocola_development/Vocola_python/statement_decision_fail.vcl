### 
### Invalid statement starts:
### 

)   ; panic=action;
]   ; panic=action;
|   ; panic=action;
,   ; panic=action;
;   ; panic=action;
=   ; panic=action;
:=   ; panic=action;
word )   ; panic=action;
word ]   ; panic=action;
word |   ; panic=action;
word ,   ; panic=action;
word ;                      # semantic error only
word word )      ; panic=action;
word word ]      ; panic=action;
word word |      ; panic=action;
word word ,      ; panic=action;
word word :=     ; panic=action;
word "word" )      ; panic=action;
word "word" ]      ; panic=action;
word "word" |      ; panic=action;
word "word" ,      ; panic=action;
word "word" :=     ; panic=action;
f( ]      ; panic=action;
f( |      ; panic=action;
f( ,      ; panic=action;
f( ;      ; panic=action;
f( :=     ; panic=action;
f( =      ; panic=action;
f(b ]      ; panic=action;
f(b ;      ; panic=action;
f(b :=     ; panic=action;
f(b =      ; panic=action;
f(b) )      ; panic=action;
f(b) ]      ; panic=action;
f(b) |      ; panic=action;
f(b) ,      ; panic=action;
f(b) ;      ; panic=action;
