###
### Commands for writing Vocola macros
###

include "string.vch";


## 
## Macros for typing SendDragonKeys keystroke commands:
## 

include "keys.vch";

Quote(keys) := PrintablesToKeys($keys);


  # just pressing a modifier key alone:
keycode (shift|control=ctrl|alt|Windows=win) [(hold|release)] = 
    Quote({$1 When($2, _$2)});

keycode [<extended_modifiers>] <non> [2..20] = Quote(Key($1, $2, When($3,_$3)));
keycode                        <non> (hold|release) = Quote(Key("", $1, _$2));

keycode [<extended_modifiers>] <prn> [2..20] = 
    When($1$3,
         Quote(Key($1, $2, When($3,_$3))),
            # use a instead of {a} where possible...
	 Quote(Replace(PrintablesToKeys($2)," ",{space})));



## 
## Other Vocola source code items:
## 

Wait (0 | 50)                        = "Wait($1)";
Wait 1..20 (hundred=00|thousand=000) = "Wait($1$2)";

message box = 'MsgBoxConfirm("", 64, "Debug info")'
                  {left_ Len('", 64, "Debug info")') };
