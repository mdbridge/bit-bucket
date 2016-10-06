### 
### Macro for comparing two documents from similarity HTML output
### 

LeftSide()  := SetMousePosition(0, 50,990) ButtonClick(1, 1);
RightSide() := SetMousePosition(0,850,990) ButtonClick(1, 1);

OpenPair() := {shift+F10}n Wait(2000)
	     SetMousePosition(1,50,10) RememberPoint()
	     SetMousePosition(0, 50, 50)
	     DragToPoint()

	     SetMousePosition(0,50,10) ButtonClick(1, 1)

	     {tab} {shift+F10}n Wait(2000)
	     SetMousePosition(1,50,10) ButtonClick(1,1) RememberPoint()
	     SetMousePosition(0, 850, 50)
	     DragToPoint()

	     LeftSide();


pair 1..100 = {F6}{F6} {tab} "{tab " Eval("($1-1)*2") } OpenPair();


pair 1..9 0..9 0..9 = {F6}{F6} {tab} "{tab " Eval("($1*100+$2*10+$3-1)*2") }
		      OpenPair();

pair here = ButtonClick(2,1)  Wait(1000) {esc} OpenPair();


close pair = LeftSide() {alt+F4}   RightSide() {alt+F4};

left side  = LeftSide();
right side = RightSide();

pair (down={PgDn} | up = {PgUp} | top = {ctrl+home}) = LeftSide() $1 RightSide() $1;

pair (down={PgDn} | up = {PgUp} | top = {ctrl+home}) times 1..50 =
		  Repeat($2, LeftSide() $1 RightSide() $1 Wait(1000));
