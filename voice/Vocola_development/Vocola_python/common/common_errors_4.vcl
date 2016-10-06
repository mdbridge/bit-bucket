### 
### Common syntax errors made when writing Vocola 2 programs, part IV.
### 

call(argument):= action;
call_2(argument):= action;


## 
## Unmatching delimiters:
## 

a command = call(argument
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(argument,
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(argument call_2(argument)
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(argument call_2(argument);
another = keystrokes and more keystrokes;
resynchronize = here;


some words are [optional = action;
another = keystrokes and more keystrokes;
resynchronize = here;

some words are optional] = action;
another = keystrokes and more keystrokes;
resynchronize = here;



## 
## Unquoted words ending in a colon:
## 

please type <list> = C: $1;
another = keystrokes and more keystrokes;
resynchronize = here;

please type <list> = call(C: $1);
another = keystrokes and more keystrokes;
resynchronize = here;


<symbol> := (  under = _ | colon = : );
another = keystrokes and more keystrokes;
resynchronize = here;

<symbol> := (  under = _ | colon = : | star = *);
another = keystrokes and more keystrokes;
resynchronize = here;



## 
## Unquoted words containing reserved characters:
## 

a command = one|two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one|two);
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one,two;
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one;two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one;two);
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one=two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one=two);
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one:two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one:two);
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one(two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one(two);
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one)two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one)two);
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one[two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one[two);
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one]two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one]two);
another = keystrokes and more keystrokes;
resynchronize = here;


a command = one:=two;
another = keystrokes and more keystrokes;
resynchronize = here;

a command = call(one:=two);
another = keystrokes and more keystrokes;
resynchronize = here;
