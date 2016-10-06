### 
### Voice commands for Civilization V under direct X 11
### 
### 
### Requires corporate firewall is down (no VPN), but otherwise appears to work
### 
### Need to turn off RSIguard; move Dragon bar, results box to other monitor;
###         disable mouse trails
### 

$set MaximumCommands 3;


## 
## SmartNav commands:
## 

pause     = SendSystemKeys({f9});

  # need to make f8 sticky:
precision = SendSystemKeys({f8});
click     = Mouse.Click() SendSystemKeys({f8});



## 
## Overall gameplay:
## 

# command mode (on|off)

quick save game = {f11};
      save game = {ctrl+s};
      load game = {ctrl+l};

game options = {ctrl+o};


encyclopedia = {f1};

(economic={f2}|military={f3}|diplomacy={f4}|tech={f5}|social={f6}
|notification={f7}|victory={f8}|demographic={f9}|strategic={f10}) info = $1;

hex grid  = G;

game menu = {esc};



## 
## Changing the viewpoint:
## 

zoom (out=-|in=+) = $1;

capital city = {home};

(resource=r|yield=y) icons = {ctrl+$1};


  # these scroll the visible window:
<direction> := (left|right|soar=up|down); 

<direction> 1..20 = SendSystemKeys({$1_$2});



## 
## Selecting units to give orders to:
## 

skip     unit   = {space};

previous unit   = ','; 
next     unit   = .;

(end|next) turn = {enter};


## 
## Giving orders to units:
## 

here             = Mouse.Click();
there            = Mouse.Click(right);

move             = m;
ranged attack    = b;
explore          = e;           # automated
alert            = a;
sleep            = f;
do nothing       = {space};

set up artillery = s;
fortify          = h;
