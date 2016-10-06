### 
### SmartNav voice commands:
### 

WithPause(commands) := SendSystemKeys({f9}) $commands SendSystemKeys({f9});


SmartNav pause     = SendSystemKeys({f9});

  # need to make f8 sticky:
SmartNav precision = SendSystemKeys({f8});
SmartNav click     = Mouse.Click() SendSystemKeys({f8});

SmartNav center    = SendSystemKeys({f12});



## 
## Current Smart Nav settings:
## 

# movement: 7, 7 
# motion: 45
# positioning: absolute
# clicking: left ability switch -> right-click
# precision: sticky



## 
## User-designated mouse touch points  that work when the SmartNav is on
## 

<n> := 1..50;

set  smart <n> = Variable.Set($1, Mouse.Position());
show smart <n> = Mouse.Go(Variable.Get($1));

     smart <n> = SendSystemKeys({f9})
                   Mouse.Click(Variable.Get($1))
                 SendSystemKeys({f9});
