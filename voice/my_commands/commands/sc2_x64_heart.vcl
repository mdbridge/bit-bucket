### 
### Voice commands for StarCraft II: Heart of the swarm
### 

include "letters.vch";
include "optional.vch";
include "printables.vch";

$set MaximumCommands 3;


# Will want to exit RSIGuard

# Set game options: 
#   show health bars for damaged units, flier markers on, hot keys,
#   game clock on, display build grid



### 
### SmartNav commands:
### 

# manual: f9 = pause, f8 = precision, f12 = center (useless w/ absolute?)

pause     = SendSystemKeys({f9});

precision =               SendSystemKeys({f8}); # need to make f8 sticky
click     = Mouse.Click() SendSystemKeys({f8});



###
### Overall gameplay:
### 

# command mode (on|off)

game menu   = {f10};
pause game  = {f10};
save  game  = {f10} Wait(100) SendSystemKeys(v);
load  game  = {f10} Wait(300) l;

go back     = b;

message log = {f11};

  # these are not bound by default; fixable by changing hotkeys?
#normal speed  = SendSystemKeys( Repeat(10, -)               );
#speed 1..10   = SendSystemKeys( Repeat(10, -) Repeat($1, +) );

toggle terrain = {alt+t};  # toggle mini map information



# fallbacks for new/rare units/buildings:

key <prn>	= PrintablesToKeys($1);

<letter> [1..9] = REPEAT($2, PrintablesToKeys($1) Wait(50));



### 
### Changing current viewpoint:
### 

center    = {ctrl+f};
follow me = {ctrl+shift+f};  # camera follows selected units

# space: center on last transmission

town center = {backspace};

  # these scroll the visible window:
<direction> := (left|right|soar=up|down); 
<direction> 1..20 = SendSystemKeys({$1_$2});

  # optional count is seconds:
spin (left=Ins|right=Del) [1..20] = 
    Keys.SendInput({$1_hold}) REPEAT($2,Wait(1000)) Keys.SendInput({$1_release});

(raise={PgUp}|lower={PgDn}|highest={home}|lowest={end}) viewpoint = $1;


## 
## Map points:
## 

<map_key> := ( 1=f5 | 2=f6 | 3=shift+f5 | 4=shift+f6 | 5 = shift+f8 
	     ); #| 6 = f7 | 7 = shift+f7 | 8 = f8 );

set map <map_key> = {ctrl+$1};
    map <map_key> = {     $1};



### 
### Selecting units/groups of units:
### 

idle worker      =      {f1} {ctrl+f};
all idle workers = {ctrl+f1} {ctrl+f};
entire army      = {f2};
hero             = {f3};

you all          = Mouse.Click(right);    # select all (up to ?) of type

<group> := (group | unit);

set      <group> 0..9 = {ctrl+$2};
add [to] <group> 0..9 = {shift+$2};
         <group> 0..9 = $2;

# (shift-)tab: cycle between units of one type in a currently selected
#              mixed group of units

# shift touch: toggle units membership in selected group

# can point to a unit's portrait when selected



### 
### Non-building unit commands:
### 

## Commands good for basically any unit:

      # "there" on friendly unit means follow it from now on
    there           =                      Mouse.Click(right);
and there           =          ShiftKey(1) Mouse.Click(right);
    here            = Wait(50)             Mouse.Click();

move                = m;
patrol              = p;
stop                = s;
(stand ground|hold) = h;

attack              = a;
charge              = a Wait(50) Mouse.Click();


## Commands good for general unit categories:

# workers:
(harvest|gather)                              = g;
(return with goods|return goods|return cargo) = c;

# transports:
unload unit          = o;
unload [all] [units] = d;

# all Zerg, including crawlers:
(burrow | unburrow | bury | unbury | root | unroot) = r;


## Unit-specific commands:

<hero> := (
   # Queen of Blades (campaign):
      blast         = q
    | grip          = w
    | baneling rain = e
    | heal          = e
);
<hero> = {f3} $1;


# Hydralisks:
frenzy = t;  # campaign only

# Queens:
transfuse	= t;   # auto on for campaign
  # creep tumors: can select several then repeatedly do "creep here"...
[spread] creep	= c;  # also works for creep tumors

# Infestors:
dominate	= e;
fungal		= f;
consume		= c;  # vipers also

# Vipers:
abduct          = d;
disabling cloud = b;



### 
### Creating buildings:
### 

<building> := ( 
  # Zerg:
      hatchery		= h
    | extractor		= e | gas     = e
    | spawning pool	= s
    | evolution chamber = v
    | roach warren	= r
    | baneling nest	= b
    | spine crawler	= c
    | spore crawler	= a

  # Terran:
    | command center	= c 
    | supply depot	= s
    | refinery		= r 
    | barracks		= b                    # requires command center
    | bunker		= u                    # requires barracks
    | engineering bay	= e                    # requires command center
    | missile turret	= t | turret  = t      # requires engineering bay
    | sensor tower	= n | tower   = n      # requires engineering bay
    );

<advanced_building> := ( 
  # Zerg:
      hydralisk den	= h                    # requires lair, can upgrade for impalers [C]
    | infestation pit	= i
    | spire		= s
    | nydus network	= n
    | ultralisk cavern	= u

  # Terran:
    | factory		= f                    # requires barracks
    | armory		= a                    # requires factory
    | starport		= s                    # requires factory
    | fusion core	= c                    # require spaceport
    | ghost academy	= g | academy = g      # requires barracks
    );

## done by workers:
build          structure  = b;
build advanced structure  = v;
build <building>          = b Wait(200) $1;
build <advanced_building> = v Wait(200) $1;



### 
### Creating units:
### 

<train> := ( build | train | make | birth );

# rally points can be a unit
[set]          rally  = y;  # also right-click from hatchery
[set] workers [rally] = g;  # heart of the swarm only


## From larva:

larva    = s;
my larva = 9 Wait(50) s;

<larva_unit> := (
      drone	 = d
    | overlord	 = v
    | zergling	 = z | ling = z # can morph later to baneling
    | aberration = b            # [C]
    | roach	 = r
    | hydralisk	 = h            # can morph later to impaler [C]
    | infestor	 = f
    | ultralisk	 = u
    | swarm host = a
    | mutalisk	 = t            # can morph later to broodlord [C]
    );

[<train>] <larva_unit> [1..10] = REPEAT($3, $2 Wait(50));


## from hatchery:

<train> queen = q;



### 
### Strategy notes:
### 

# try double-click buildings, or shift click to add?

# focus fire on one enemy at a time
# focus on soft (easy) targets first

# put supply depots in front of bunkers
# 6:1 marine:medic is about right
# marauders are good versus armor



### 
### Wings of liberty/Terran/Protoss commands:
### 

## Unit-specific commands:

# SCV (toggle):
auto repair = Mouse.Click(right, 1586,  942);

# marines/marauders:
stim                 = t;

# siege tanks:
tank  mode           = d;
siege mode           = e;

# Vikings:
fighter mode         = e;
assault mode         = d;

# ghosts:
EMP                  = e;
snipe                = r;
nuke                 = n;

# ravens:
auto turret          = t;
point defense drone  = d;
seeker missile       = r;

# cloaking units:
cloak                = c;
de-cloak             = d;

# Campaign Protoss:
blink                = b;
void                 = v;


## Buildings:

build flame turret = b Wait(200) f;  # campaign only, purchased
build Merc center  = v Wait(200) m;  # campaign only

# Barracks add-ons:
build tech lab = x;
build reactor  = c;

# other:
(lower|raise) [supply] deput = r;

(lift|take) off = l;
land            = l;

# orbital command:
mule = e;
scan = c;


## Creating units:

<Terran_unit> := (
  # Command center commands:
      SCV              = s

  # Barrack commands:
    | marine           = a
    | marauder         = d
    | reaper           = r
    | ghost            = g   # requires ghost academy
   
    | medic            = e   # campaign only, requires tech lab
    | fire bat         = f   # campaign only
    | spectre          = s   # campaign only, requires ghost academy


  # Factory commands:
    | hellion          = e
    | [siege] tank     = s
    | Thor             = t   # requires armory
   
    | vulture          = v   # campaign only
    | diamondback      = d   # campaign only
    | Goliath          = g   # campaign only

  # Starport commands:
    | Viking           = v
    | Medivac dropship = d
    | Banshee          = e
    | Raven            = r
    | Battlecruiser    = b   # requires fusion core
   
    | wrath            = w   # campaign only  # ???
);

<Protoss_unit> := (
  # Nexus:
      probe            = e

  # Gateway:
    | Zealot           = z
    | Stalker          = s
    | Sentry           = e
    | Dark templar     = d
    | High templar     = t
);

<train> <Terran_unit>  = $1;
<train> <Protoss_unit> = $1;
