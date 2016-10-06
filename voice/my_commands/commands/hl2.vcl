### 
### Voice commands for portal
### 

# 
# Set options:
# 
#   walk forward = left click (was w)
#   blue portal = space
#   jump = z
# 
#   mouse sensitivity: 9.9 [was 18.3]
# 
# SmartNav: portal profile (non-administrator, relative, speed 4/5)
# 	    MUST set mouse override delay (behavior tab) to 0!
# 
# 
# Need VPN down for stream account online mode so can get achievements
# 


jump           = z;

fire blue      = {space};
fire yellow    = Mouse.Click(right);

quick save     = {f6};
quick load     = {f9};

(grab|drop) it = e; 


  # hold given key down for $milliseconds:
Pulse(key, milliseconds) := Keys.SendInput({$key _hold})    Wait($milliseconds)
                            Keys.SendInput({$key _release});

backwards             [1..90] = Pulse(s,    When($1,$1 0,300));

walk (left=a|right=d) [1..90] = Pulse($1,   When($2,$2 0,300));

turn (left|right)     [1..90] = Pulse($1,   When($2,$2 0,300));

duck                  [1..90] = Pulse(ctrl, When($1,$1 00,500));


crouch   = Keys.SendInput({ctrl_hold});
stand up = Keys.SendInput({ctrl_release});


third person = {Ins}  "sv_cheats 1"{enter}  "thirdperson"{enter}  {esc};
