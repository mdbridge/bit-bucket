### 
### Voice commands for testing the new Unimacro built-in
### 

include "Unimacro.vch";


###########################################################################
#                                                                         #
# Pressing keys/typing/producing characters:                              #
#                                                                         #
###########################################################################

open Explorer window            = WINKEY(e);

type ASCII character            = A(77);  # M
test unicode delta              = Unimacro("U Delta");
test date                       = DATE();

try shortcut for SendSystemKeys = SSK('x xy{tab}z');
#try quoting semicolons          = SSK('x x;y{tab}');  # FAILING


###########################################################################
#                                                                         #
# Waiting:                                                                #
#                                                                         #
###########################################################################

test             wait 1..10 = SetMicrophone(0) WW($1) SetMicrophone(1);
test alternative wait 1..10 = SetMicrophone(0) W1($1) SetMicrophone(1);


Nudge() := MP(2,4,-1,0);  # also test relative mouse motion...

test wait speed =
	Repeat(5, Nudge() W()  )
	Repeat(5, Nudge() SW() )
	Repeat(5, Nudge() LW() );
	
# VW() is tested by travel around the... below


wait for       xterm  = PMP() WWT("xterm alpha") MSG("found xterm!");
wait for upper xterm  = PMP() WWT("xterm Alpha") MSG("found xterm!");


wait for new window = RW() WTC()                      MSG('got new window');
do I switch         = RW() WTC1(Eval('int(10/0.05)')) MSG('got new window');



###########################################################################
#                                                                         #
# Mouse commands:                                                         #
#                                                                         #
###########################################################################

<type> := ( absolute         = PMP()    | relative         = PRMP()  |
	    absolute corners = PMP1()   | relative corners = PRMP1() |
	    all              = PALLMP()
	  );

print <type> mouse information = $1;


<area> := (world = 0 | window = 1 | client area = 5 );

travel around the <area> = 
        RM()
        RMP($1, +0.01, +0.01, noclick) VW()
        RMP($1, -0.01, +0.01, noclick) VW()
        RMP($1, -0.01, -0.01, noclick) VW()
        RMP($1, +0.01, -0.01, noclick) VW()
        RMP($1, +0.01, +0.01, noclick) VW()
	CANCELMOUSE();

travel in the <area> = 
        RM()
        MP($1, 100, 100, noclick) VW()
        MP($1, 200, 100, noclick) VW()
        MP($1, 200, 200, noclick) VW()
        MP($1, 100, 200, noclick) VW()
        MP($1, 100, 100, noclick) VW()
	CANCELMOUSE();


<button> := left | middle | right | default="";  
<action> := click   | double-click=double | hold=down  | lift=up 
          | release | default="";

<action> the <button> mouse button = MP(2, 0, 0, $2$1);
do not click the mouse button      = MP(2, 0, 0, no click);


hold button down      = MDOWN();
release mouse buttons = ENDMOUSE();

move upwards then back
	= RM() MDOWN() VW() MP(2,0,-30,noclick) VW() CANCELMOUSE();



###########################################################################
#                                                                         #
# Switching between windows/applications:                                 #
#                                                                         #
###########################################################################

call bring up Internet explorer = BRINGUP(iexplore);
call bring up macro messages    = BRINGUP(messages);

mark current window = RW();
marked window       = RTW();


###########################################################################
#                                                                         #
# Closing windows/applications:                                           #
#                                                                         #
###########################################################################

test kill window   = KW();
test kill document = KW1({ctrl+f4});


###########################################################################
#                                                                         #
# Dealing with the Windows clipboard                                      #
#                                                                         #
###########################################################################

save    Windows clipboard = CLIPSAVE();
restore Windows clipboard = CLIPRESTORE();

place between parens = UseClipboard('{ctrl+c}', '(){left}{ctrl+v}{right}');


###########################################################################
#                                                                         #
# Communicating with the user:                                            #
#                                                                         #
###########################################################################

display a message    = MSG('This is a test; can you read it?');
display two messages = MESSAGE(first) MESSAGE(second);

ask a question       = YESNO('is today Friday?', Great!);



###########################################################################
#                                                                         #
# Miscellaneous actions:                                                  #
#                                                                         #
###########################################################################

what version is my Emacs = EMACS(version);

  # "U.S. Customs" is a single word so only the first of these will work:
test smart HeardWord = HW("U.S. Customs");
test dumb  HeardWord = HW("U.S., Customs");



###########################################################################
#                                                                         #
# Other tests:                                                            #
#                                                                         #
###########################################################################

run unimacro command = Unimacro(foobar);

test meta action     = Unimacro(<<filesave>>);
test line delete     = Unimacro("<<selectline>><<linedelete>>"); 

test true            = Unimacro("T; this always appears");
test false           = Unimacro("F; this never appears");

test print Okay      = Unimacro(<<printstart>>) {enter};

