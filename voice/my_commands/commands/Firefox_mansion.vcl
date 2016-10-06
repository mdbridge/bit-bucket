## 
## These are for the Phantom mansion:
## 
## http://bayareajenn.livejournal.com/199379.html
## 
## Last time used 1152 x 864 resolution.
## 

$set MaximumCommands 4;


mansion:

space = SendSystemKeys({space});


#<direction> := ( left | right | down | soar=up );
#
#<direction>       =            SendSystemKeys({$1})            Wait(100);
#<direction> 1..20 = Repeat($2, SendSystemKeys({$1}) Wait(100)) Wait(100);



include "control.vch";


Choose(normal, confused) := If(Variable.Get(mansion:confused, "0"), 
	                       $confused, $normal);

I am     confused = Variable.Set(mansion:confused, 1);
I am not confused = Variable.Set(mansion:confused, 0);

am I confused = 
    MsgBoxConfirm(Variable.Get(mansion:confused,"0"),64,"1 below means confused:");

<direction> := ( left=Choose(left,right) | right=Choose(right,left) 
	       | down=Choose(down,up)    | soar=Choose(up,down) );

<direction> [1..20] = When($2,Repeat($2,SendSystemKeys({$1}) Wait(100))) Wait(100);
