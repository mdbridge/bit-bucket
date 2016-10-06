### 
### Global commands for using the mouse:
### 

$set MaximumCommands 2;
include "optional.vch";



## 
## Basic clicking:
## 

<modifier> := ( shift=1 | control=2 | alt=3 );

<touch>    := ( touch=1 | right touch=2 | middle touch=4 );


       <touch> [times 1..50] = REPEAT($2, ButtonClick($1, 1) Wait(50));
double <touch>               =            ButtonClick($1, 2);


[<modifier>] <modifier> <touch> = When($1,ShiftKey($1)) ShiftKey($2) 
	     			  ButtonClick(1, 1);



## 
## Holding the mouse buttons or modifier keys:
## 

hold (left=LeftButton | right=RightButton | shift | control=ctrl | alt) = 
    Keys.SendInput({$1_hold});

release = Keys.SendInput({LeftButton_release}{RightButton_release}
	                 {shift_release}{ctrl_release}{alt_release});


## 
## Remembering points:
## 

include "letters.vch";
include "string.vch";

<kind> := (absolute | window | interior);

set [<kind>] <letter> point = 
   Variable.Set($2, Mouse.Position(When($1,$1,absolute)));

	       <letter>	point = Mouse.Go(Variable.Get($1));
code for       <letter>	point = "Mouse.Go("    Variable.Get($1) ")";
code for click <letter> point = "Mouse.Click(" Variable.Get($1) ")";

  # <<<>>>
code for       <kind> point = "Mouse.Go("    Mouse.Position($1) ")";
code for click <kind> point = "Mouse.Click(" Mouse.Position($1) ")";



## 
## Moving the mouse by percentages:
## 

  # get appropriate screen resolution:
include "environment.vch";
include "numbers.vch";

X(x) := Eval('int(' XSize() * 1.0 / 100 * $x ')');
Y(y) := Eval('int(' YSize() * 1.0 / 100 * $y ')');

# These functions assume all monitors are the same resolution

     Position(x,y) := Mouse.Go(X($x), Y($y));

 LeftPosition(x,y) := Mouse.Go(X($x), Y($y))
                      Mouse.Go(relative, Eval(- XSize()), 0);

RightPosition(x,y) := Mouse.Go(X($x), Y($y))
                      Mouse.Go(relative, XSize(), 0);

#mouse       <my0to99> by <my0to99> =      Position($1, $2);
#mouse left  <my0to99> by <my0to99> =  LeftPosition($1, $2);
#mouse right <my0to99> by <my0to99> = RightPosition($1, $2);



## 
## Moving the mouse slightly:
## 

nudge (right=""|left=-) <my0to99>  = Mouse.Go(relative, $1$2, 0);
nudge (down=""|up=-)    <my0to99>  = Mouse.Go(relative, 0, $1$2);



## 
## Commands usefully combined with mouse operations:
## 

(leap|page) (up=PgUp|down=PgDn) [2..10] = {$2 When($3,_$3)};



## 
## Experiments:
## 

grab    title = Mouse.Go(window, 100, 20) Keys.SendInput({LeftButton_hold});
release title = Keys.SendInput({LeftButton_release});


local mouse <my0to99> by <my0to99> = Mouse.SetScaled("$1,$2");

Meridian <my0to99> = Mouse.SetScaled("50,$1");

      center point = Mouse.SetScaled("50,50");
mid   center point = Position(50, 50);
right center point = RightPosition(50, 50);


go scrollbar = Mouse.SetScaled("100,93") Mouse.Go(relative, -20, 0);

wheel (up=""|down="-") 1..100 = Kludge.Wheel($1 $2);
tick wheel (up=""|down="-") 1..100 = Repeat($2,Kludge.Wheel($1 1) Wait(50));
