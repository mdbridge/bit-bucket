### 
### Global commands for using the mouse:
### 

$set MaximumCommands 2;



## 
## Basic clicking:
## 

<modifier> := ( shift=1 | control=2 | alt=3 );

<touch>    := ( touch=2 | right touch=1 | middle touch=4 ); # <<<>>>


       <touch>                        =            ButtonClick($1, 1);
double <touch>                        =            ButtonClick($1, 2);
       <touch> times 1..50            = Repeat($2, ButtonClick($1, 1) Wait(50));


           <modifier> <touch>         =              ShiftKey($1) ButtonClick(1, 1);
<modifier> <modifier> <touch>         = ShiftKey($1) ShiftKey($2) ButtonClick(1, 1);



## 
## Remembering points:
## 

include "letters.vch"; # 9e7ac9a3799a0abd9e068515aa9204f8 
include "string.vch"; # beb5b2c92bd68c5c145486d38b3e3dcc 


     set <letter> point               = Variable.Set($1, Mouse.Get());
         <letter> point               = Mouse.Set(Variable.Get($1));

mouse_code_for_move(position) :=
        Variable.Set(_temp, $position)
        'SetMousePosition(0, ' Split(Variable.Get(_temp), ",", 0) ', '
                               Split(Variable.Get(_temp), ",", 1) ')';

code for <letter> point               = mouse_code_for_move(Variable.Get($1));



## 
## Moving the mouse by percentages:
## 

  # get appropriate screen resolution:
include "environment.vch"; # 5efa7482fa6787c9ae002d856df10c23 
include "numbers.vch"; # 63d30fabbe236ecedaca661398ee790d 

X(x) := Eval('int(' XSize() * 1.0 / 100 * $x ')');
Y(y) := Eval('int(' YSize() * 1.0 / 100 * $y ')');

# These functions assume all monitors are the same resolution

     Position(x,y) := SetMousePosition(0, X($x), Y($y));

 LeftPosition(x,y) := SetMousePosition(0, X($x), Y($y))
                      SetMousePosition(2, Eval(- XSize()), 0);

RightPosition(x,y) := SetMousePosition(0, X($x), Y($y))
                      SetMousePosition(2, XSize(), 0);

mouse       <my0to99> by <my0to99>    =      Position($1, $2);
mouse left  <my0to99> by <my0to99>    =  LeftPosition($1, $2);
mouse right <my0to99> by <my0to99>    = RightPosition($1, $2);



## 
## Moving the mouse slightly:
## 

nudge (right=""|left=-) <my0to99>     = SetMousePosition(2, $1$2, 0);



## 
## Commands usefully combined with mouse operations:
## 

(leap|page) (up=PgUp|down=PgDn)       = {$2};
(leap|page) (up=PgUp|down=PgDn) 2..10 = {$2_$3};


## 
## Experiments:
## 

grab    title = SetMousePosition(1, 100, 20) Mouse.Hold_right();
release title = Mouse.Release_right();
