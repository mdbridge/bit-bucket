###
### Website specific commands for Internet Explorer (version 7)
###

include "iexplore.vch";



##
## Commands for libraries:
##

next search = HeardWord(type, text) HeardWord(select, line);



## 
## Commands for Gmail:
## 

  # a button, not a link...
archive that = ClickFirst(Archive);



##
## Commands for livejournal:
##

login to live journal =
	HeardWord(click, username) {space}{backspace}
	HeardWord(click, remember, me)
	{enter};



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
	     Wait(1000) # wait for menu to go away before move mouse...
             SetMousePosition(5, Eval($x+193), Eval($y+213))
             ButtonClick(1,1);

push left  light = Light(700,  650);
push right light = Light(1000, 650);



## 
## Commands for TechCon:
## 

include "environment.vch";

X(x) := Eval('int(' XSize() * 1.0 / 100 * $x ')');
Y(y) := Eval('int(' YSize() * 1.0 / 100 * $y ')');


     Position(x,y) := SetMousePosition(0, X($x), Y($y));

RightPosition(x,y) := SetMousePosition(0, X($x), Y($y))
	              SetMousePosition(2, XSize(), 0);

retreat       = RightPosition(57, 16) ButtonClick(1, 1);
download left = Position(50, 50)      ButtonClick(1, 1) 
                HeardWord(click, here, to, download);


issue invitation = "chat about scaling out poster?";
