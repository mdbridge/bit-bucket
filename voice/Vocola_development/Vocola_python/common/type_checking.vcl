### 
### Every type checking error (modulo forward reference checking, no commands):
### 

<other> := ( dummy );


# context statements:

  one
  |  # ok =
    not ok =  |
  fred :
panic = to here;


# variable definitions:

<wrong      := (menu);
<closer!>   := (menu);
<_anything> := (menu);


# function definitions:

wrong!(argument)     := actions;
wrong.call(argument) := actions;
once()               := is okay;
once()               := twice is not;
panic = to here;


# formals:

function(bad!) := actions;


# directives:

unknown directive;     panic = to here;
include;               panic = to here;
include two words;     panic = to here;
$set;                  panic = to here;
$set one;              panic = to here;


# terms:

[ optional ] = error;
<_anything>  = error;


# simple terms:

try <closer!> for lunch = today;


# menu bodies:

<list> := ( | empty );
<list> := ( empty | );
<list> :=  | empty ;
<list> :=  empty | ;


# actions:

1..9  = **$1**$2**;
F(x) := **$x**$y**;


# calls:

do it         = bad!(argument);
bad extension = unknown.unknown(argument);
bad function  = unknown(argument);
too few       = SendDragonKeys();
too many      = ButtonClick(1,2,3,4);


# verifying referenced menus:

<list> := ( one two [optional]  bad ); panic = to here;
<list> := ( one two <other>     bad ); panic = to here;
<list> := ( one two <_anything> bad ); panic = to here;
<list> := ( one two (one|two)   bad ); panic = to here;
<list> := ( one two 1..99       bad ); panic = to here;

<list> := ( 1..99     = actions );     panic = to here;
<list> := ( 1..99 )   = actions;       panic = to here;
<list> := (( 1..99 )) = actions;       panic = to here;

<list> :=  1..99     | other;          panic = to here;
<list> :=  (1..99)   | other;          panic = to here;
<list> :=  (1..99    | other);         panic = to here;
<list> :=  ((1..99)) | other;          panic = to here;

<list> := ( ( 1 = 2) = 3);             panic = to here;
<list> := (( 1 = 2 )) = 3;             panic = to here;
<list> := ( one ( 1 = 2) = 3);         panic = to here;


  # these used to produce different errors in the Perl version:
<list> := ( 1..99 = $1 );              panic = to here;
<list> := ( <other> );                 panic = to here;
<list> := ( <_anything> );             panic = to here;
