###
### Commands for Dragon NaturallySpeaking's Command Browser (DNS 10.1)
###
### Note: the command editor window is a different module, natspeak.
###

include "locale_PC.vch";

include "letters.vch";


##
## Commands usable in manage mode:
##

Group(keys)     := {alt+g}$keys;

ManageMenu(key) := {alt+a}$key;


  # bring up export dialog box with save as type .XML selected
export XML = ManageMenu(e) {tab}{down}{down}{enter}{shift+tab};



##
## Commands usable from any mode:
##

  # switch to manage mode from any mode:
ManageMode() := {alt+m}n;

  # switch to manage mode, with focus on list of commands pane:
ManagePane() := ManageMode() Group({tab});


really manage    = ManagePane();


  # edit command selected in manage view:
really edit      = ManagePane() {ctrl+s} {alt+s}e Wait(3000)
        {alt+r};

  # edit first list of command selected in manage view:
really edit list = ManagePane() {ctrl+s} {alt+s}e Wait(3000)
        {alt+m}{tab}{tab}{down} {alt+e};


  # input mini-Vocola compiler produced commands:
import out = ManageMode() {ctrl+i} Wait(1000)
             UNIX("foil:~/voice/mini-Vocola/to_DNS.xml") {enter};



CurrentGroup(key) := ManageMode() Group($key) {tab}
                     {down}{right}{up}{right};


  # switch to managing every group:
      current group all = CurrentGroup('(' {down}{up})
	Repeat(16, {space}{left}{down});

  # switch to managing the first group whose name starts with letter:
      current group <letter> = CurrentGroup('(' $1);

check current group <letter> = CurrentGroup('(' $1)
	Repeat(30, {space}{left}{down});


  # switch to managing the next group:
next current group = CurrentGroup({down});
