### 
### Voice commands for Return to Zork
### 

# 
# need to use SmartNav as administrator
# turn off RSIGuard
#
# use Zork profile: relative cursor positioning
#
# 



inventory = ButtonClick(2, 1);

WithPause(commands) := SendSystemKeys({f9}) $commands SendSystemKeys({f9});


stable inventory = WithPause(ButtonClick(2, 1));


menu             = SendSystemKeys({f1});
save game        = SendSystemKeys({f2});
load game        = SendSystemKeys({f3});
repeat message   = SendSystemKeys({f4});

full screen      = SendSystemKeys({alt+enter});  # toggle


force <_anything> = SendSystemKeys($1);
