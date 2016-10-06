###
### Website specific commands for Internet Explorer (version 6)
###

OpenURL(URL) := {ctrl+o} $URL {enter};





##
## Commands for livejournal:
##

live journal login page = OpenURL(http://www.livejournal.com/login.bml);

login to live journal =
	{tab}{space}{tab}{tab};


  #  click one of multiple previous links:
really previous =
	HeardWord(click, Previous)
	HeardWord(choose, 1);

really lower previous =
	HeardWord(click, previous)
	HeardWord(choose, 1);

really next =
	HeardWord(click, Next)
	HeardWord(choose, 1);

escape previous =
	HeardWord(press, escape)
	HeardWord(click, Previous)
	HeardWord(choose, 1);


## 
## Commands for Gmail:
## 

archive that = HeardWord(Archive) HeardWord(choose,1);



## 
## Commands for speech computing:
## 




## 
## Voice commands for turning on the lights at work:
## 

prepare lights =
	OpenURL('http://light-control.hpl.hp.com/lighting/showFlash.aspx'
	  '?path=1U.swf&param1=light-control.hpl.hp.com/lighting/fsproxy.aspx')
	Wait(500) {F11};

finish lights = {F11};


Light(x,y) :=   # click on bulb button:
	      SetMousePosition(5, $x, $y)
              ButtonClick(1,1) ButtonClick(1,1)

	        # Select all lights on:
	      SetMousePosition(5, Eval($x+50),  Eval($y+120))
              ButtonClick(1,1) 
	      "All Lights On" {enter}
#	      {up_4} Wait(5000) {down_2} Wait(1000) {enter}

	        # Press green checkmark:
             SetMousePosition(5, Eval($x+193), Eval($y+208))
             ButtonClick(1,1);

push left  light = Light(700,  650);
push right light = Light(1000, 650);



## 
## Commands for vampires game
## 

#Vampires:

<section> := ( 
               coffin     = stats.php 
             | stats      = stats.php 
             | news       = index.php 
             | missions   = jobs.php 
             | combat     = fight.php 
             | bazaar     = inventory.php 
	     );


vampire <section> = {ctrl+o}http://apps.facebook.com/vampiresgame/$1{enter}
                    SetMousePosition(5, 10, 100);


vampire blood     = HeardWord(click, blood);
vampire deposit   = HeardWord(click, deposit);

vampire health    = HeardWord(click, health) Wait(1000) {tab_2};
vampire replenish = HeardWord(click, replenish, health);



Move(keys) := SetMousePosition(5, 10, 100) ButtonClick(1, 1) $keys;

vampire top        = Move({ctrl+home});

vampire down       = Move({PgDn});
vampire down 2..10 = Move({PgDn_$1});

vampire up         = Move({PgUp});
vampire up 2..10   = Move({PgUp_$1});


## 
## Commands for reddit:
## 

really forward = HeardWord(click, next);




## 
## Commands for TechCon:
## 

  # get appropriate screen resolution:
include "environment-$COMPUTERNAME.vch";

X(x) := Eval('int(' XSize() * 1.0 / 100 * $x ')');
Y(y) := Eval('int(' YSize() * 1.0 / 100 * $y ')');


     Position(x,y) := SetMousePosition(0, X($x), Y($y));

RightPosition(x,y) := SetMousePosition(0, X($x), Y($y))
	              SetMousePosition(2, XSize(), 0);

retreat = RightPosition(57, 16) ButtonClick(1, 1);
download left = Position(50, 50) ButtonClick(1, 1) HeardWord(click, here, to, download);


issue invitation = "chat about scaling out poster?";
