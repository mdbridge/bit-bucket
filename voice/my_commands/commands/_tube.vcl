###
### Commands for Tube project, global
###

include "switch.vch";


NewPrompt(window_suffix) := SwitchToApp("command prompt $window_suffix",
                              'cmd.exe /k "title command prompt $window_suffix"');

make (Excel charts=-all|all Excel charts=-all|one Excel chart="") = 
	NewPrompt(Alpha)
	UNIXfromPC(work:~/deduplication/tube/Eiger/scripts/make_Excel_charts.rb)
            " $1" {enter};
