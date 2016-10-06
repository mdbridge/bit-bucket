###
### Commands for Dragon NaturallySpeaking's Command Browser (DNS 8.1)
###
### Note: the command editor window is a different module, natspeak.
###

include "locale_PC.vch";

include "letters.vch";


##
## Commands usable from any mode:
##

  # switch to manage mode, with focus on list of commands pane
really manage = {alt+m}n{alt+r}{tab};


  # edit command selected in manage view:
really edit =
	{alt+m}n{alt+r}{tab} {alt+n}s {alt+s}e
	Wait(3000)
	{alt+r};

  # edit first list of command selected in manage view:
really edit list =
	{alt+m}n{alt+r}{tab} {alt+n}s {alt+s}e
	Wait(3000)
	{alt+m}{tab}{tab}{down} {alt+e};


  # input mini-Vocola compiler produced commands:
import out = {alt+m}n {alt+n}i Wait(1000)
             UNIX("~/voice/my_commands/to_DNS.xml") {enter};



##
## Command usable in manage mode:
##

  # switch to managing the first group whose name starts with letter:
current group <letter> =
	"{alt+m}n{alt+r}(" $1 {tab}
	{down}{right}{up}{right};

check current group <letter> =
	"{alt+m}n{alt+r}(" $1 {tab}
	Repeat(16, {space}{down});

  # switch to managing the next group:
next current group =
	{alt+m}n{alt+r}{down}{tab}
	{down}{right}{up}{right};

  # switch to managing every group:
current group all =
	"{alt+m}n{alt+r}(" {down}{up} {tab}
	Repeat(16, {space}{down});

  # bring up export dialog box with save as type .XML selected
export XML		= "{alt+n}e{tab}{down}{down}{enter}{shift+tab}";
