### 
### Voice commands for StarCraft II: Wings of Liberty
### 

$set MaximumCommands 3;



# 
# Set game options: always show health bars, fliers, hot keys
# also turned on building grid
#

# it may be possible to change game speed again if change hotkeys


# try double-click buildings, or shift click to add?


# put supply depots in front of bunkers
# focus fire on one enemy at a time
# focus on soft (easy) targets first
# 6:1 marine:medic is about right
# marauders are good versus armor



### 
### SmartNav commands:
### 

# manual: f9 = pause, f8 = precision, f12 = center (useless w/ absolute?)

pause     = SendSystemKeys({f9});

WithPause(commands) := SendSystemKeys({f9}) $commands SendSystemKeys({f9});


  # need to make f8 sticky:
precision = SendSystemKeys({f8});
click     = ButtonClick(1,1) SendSystemKeys({f8});



### 
### The normal touch with modifiers isn't reliable with this game; so
### use the AutoHotkey script Warcraft.ahk to generate them instead:
### 
### Warning: this still doesn't appear to be 100% reliable!
### 

<modifier>         := ( shift=1 | control=2 | alt=3 );

ModTouch(modifier) := SendSystemKeys({alt+ctrl+$modifier});

#<modifier> touch = ModTouch($1);


  # these don't seem to work at the moment...
  # mouse moves, but does not select
      lasso = ModTouch(8);
giant lasso = ModTouch(9);



###
### Overall gameplay:
### 

# command mode (on|off)

game menu      = {f10};
pause game     = {f10};
save  game     = {f10}s;
load  game     = {f10}l;

message log    = {f11};

  # these are not bound by default:
#normal speed  = SendSystemKeys( Repeat(10, -)               );
#speed 1..10   = SendSystemKeys( Repeat(10, -) Repeat($1, +) );

toggle terrain = {alt+t};  # toggle mini map information



include "printables.vch";

  # fallback for new units/buildings:
key <prn> = PrintablesToKeys($1);



### 
### Changing current viewpoint:
### 

center    = {ctrl+f};
follow me = {ctrl+shift+f};  # camera follows selected units

  # these need SendSystemKeys + hold for time => AutoHotkey:
spin left = {Ins};
spin right ={Del};

(raise={PgUp}|lower={PgDn}|highest={home}|lowest={end}) viewpoint = $1;

# space: center on last transmission

town center = {backspace};

  # these scroll the visible window:
<direction> := (left|right|soar=up|down); 

<direction> 1..20 = SendSystemKeys({$1_$2});


## 
## Map points:
## 

#
# built-in ones:
#
<map_key> := ( 1=f5 | 2=f6 | 3=shift+f5 | 4=shift+f6 | 5 = shift+f8 );

set map <map_key> = {ctrl+$1};
    map <map_key> = {     $1};


# 
# extended ones:
# 
GoExtended(n) := Mouse.Set(Variable.Get(map_$n)) ButtonClick(1, 1)
	         Variable.Set(last_map, Variable.Get(map_$n));                 

manually set map 6..20 = Variable.Set(map_$1, Mouse.Get()) GoExtended($1);

set map 6..20 = WithPause(Variable.Set(map_$1, Mouse.Get()) GoExtended($1));
    map 6..20 = WithPause(                                  GoExtended($1));



### 
### Selecting places/buildings:
### 

## 
## Remembered places:
## 

<place> := (
	# <building>
	     great hall=h | town hall = h | hall=h 
           | farm=f 
           | barracks=b 
           | tower=t 
           | lumber mill=l 
           | blacksmith=s 

	# <advanced_building>
           | shipyard=_s                        # requires mill
           | foundry=_f                         # requires shipyard
           | oil refinery=_r | refinery=_r      # requires shipyard

           | goblin alchemist=_a | alchemist=_a  
           | ogre mound=_o | mound=_o 

           | stables=_a
           | Gnomish inventor=_i | inventor=_i

           | dragon roost=_d | roost=_d          
           | altar of storms=_l | altar=_l       
           | temple of the damned=_t | temple=_t 

           | Gryphon aviary=_g | aviary=_g
           | mage tower=_m
           | church=_c

        # other:
           | keep=__h | castle=__h

           | exit=__e
           | mine=__m
	   | oil rig=__o | rig=__o
);

SetPlace(place, number) := 
        WithPause(
            Variable.Set(offset_$place    _$number, Mouse.Get())
            Variable.Set(map_point_$place _$number, Variable.Get(last_map))
            ButtonClick(1, 1)
        );

 GoPlace(place, number) := 
        WithPause(
            Mouse.Set(Variable.Get(map_point_$place _$number)) ButtonClick(1, 1)
            Mouse.Set(Variable.Get(offset_$place    _$number)) ButtonClick(1, 1)
        );
            

  # use set <place> immediately after an extended map point recall,
  # without moving the display window:
#set <place>      = SetPlace($1, 1 );
set <place> 1..5 = SetPlace($1, $2);

#    <place>      =  GoPlace($1, 1 );
#    <place> 1..5 =  GoPlace($1, $2);



### 
### Selecting units/groups of units:
### 

<group> := (group | unit);

idle worker      = {f1} {ctrl+f};

you all          = ButtonClick(1, 2);    # select all (up to ?) of type

# (shift-)tab: cycle between units of one type in a currently selected
#              mixed group of units

# shift touch: toggle units membership in selected group

# can point to a unit's portrait when selected


set      <group> 0..9 = {ctrl+$2};
add [to] <group> 0..9 = {shift+$2};
         <group> 0..9 = $2;


# 
# Clicking on a unit's portrait:
#
# (screen resolution dependent, probably)
#
FacePos(x,y) := Eval(33+$x*50) ',' Eval(188+$y*50);

Face(offset) := FacePos(Eval($offset % 3), Eval($offset / 3));

position 0..8 = Mouse.Set(Face($1));  # testing...

<face> := (
    first    = 0 |
    second   = 1 |
    third    = 2 |
    fourth   = 3 |
    fifth    = 4 |
    sixth    = 5 |
    seventh  = 6 |
    eighth   = 7 |
    ninth    = 8 |

    top 1    = 0 |
    top 2    = 1 |
    top 3    = 2 |
    middle 1 = 3 |
    middle 2 = 4 |
    middle 3 = 5 |
    bottom 1 = 6 |
    bottom 2 = 7 |
    bottom 3 = 8
);

      <face> = WithPause(Mouse.Set(Face($1)) ButtonClick(1,1));
shift <face> = WithPause(Mouse.Set(Face($1)) ModTouch(1));

  # experiment; these don't work reliably at present...
odd = WithPause(
            Mouse.Set(Face(0)) ModTouch(1)
	    Mouse.Set(Face(2)) ModTouch(1)
	    Mouse.Set(Face(4)) ModTouch(1)
	    Mouse.Set(Face(6)) ModTouch(1)
	    Mouse.Set(Face(8)) ModTouch(1)
);
even = WithPause(
            Mouse.Set(Face(1)) ModTouch(1)
	    Mouse.Set(Face(3)) ModTouch(1)
	    Mouse.Set(Face(5)) ModTouch(1)
	    Mouse.Set(Face(7)) ModTouch(1)
);



### 
### Non-building unit commands:
### 

## 
## Commands good for basically any unit:
## 

    there           =                      ButtonClick(2, 1);
and there           =          ShiftKey(1) ButtonClick(2, 1);
    here            = Wait(50)             ButtonClick(1, 1);

move                = m;
patrol              = p;
stop                = s;
(stand ground|hold) = h;

attack              = a;
charge              = a Wait(50) ButtonClick(1,1);


## 
## Unit-specific commands:
## 

## SCVs:

repair                                        = r;
(harvest|gather)                              = g;
(return with goods|return goods|return cargo) = c;

  # toggle:
auto repair = SetMousePosition(0, 1586,  942) ButtonClick(2, 1);


## marines/marauders:

stim                 = t;

## siege tanks:

tank  mode           = d;
siege mode           = e;

## Vikings:

fighter mode         = e;
assault mode         = d;

## ghosts:

EMP                  = e;
snipe                = r;
nuke                 = n;

## ravens:

auto turret          = t;
point defense drone  = d;
seeker missile       = r;

## cloaking units:

cloak                = c;
de-cloak             = d;

## transports:

unload unit          = o;
unload [all] [units] = d;


## Campaign Protoss:

blink                = b;
void                 = v;



### 
### Building buildings:
### 

<building> := ( command center=c 
              | supply depot=s
              | refinery=r 
              | barracks=b                           # requires command center
              | bunker=u                             # requires barracks
              | engineering bay=e                    # requires command center
              | missile turret=t | turret=t          # requires engineering bay
              | sensor tower=n   | tower=n           # requires engineering bay

              | flame turret=f                       # campaign only, purchased
              );

<advanced_building> := ( factory=f                   # requires barracks
                       | armory=a                    # requires factory
                       | starport=s                  # requires factory
                       | fusion core=c               # require spaceport
                       | ghost academy=g | academy=g # requires barracks

                       | Merc center=m               # campaign only
                       );

## SCVs:

build          structure      = {shift+b};
build advanced structure      = {shift+v};

build <building>              = {shift+b} Wait(200) {shift+$1};
build <advanced_building>     = {shift+v} Wait(200) {shift+$1};

## Barracks:

build tech lab = x;
build reactor  = c;



### 
### Building commands other than training:
### 

(lower|raise) [supply] deput = r;

(lift|take) off = l;
land            = l;

## orbital command:

mule = e;
scan = c;



### 
### Training units:
### 

<train> := ( build | train );

set rally point          = y;


## Command center commands:

<train> SCV              = s;

## Barrack commands:

<train> marine           = a;
<train> marauder         = d;
<train> reaper           = r;
<train> ghost            = g;   # requires ghost academy

<train> medic            = e;   # campaign only, requires tech lab
<train> fire bat         = f;   # campaign only
<train> spectre          = s;   # campaign only, requires ghost academy


## Factory commands:

<train> hellion          = e;
<train> [siege] tank     = s;
<train> Thor             = t;   # requires armory

<train> vulture          = v;   # campaign only
<train> diamondback      = d;   # campaign only
<train> Goliath          = g;   # campaign only

## Starport commands:

<train> Viking           = v;
<train> Medivac dropship = d;
<train> Banshee          = e;
<train> Raven            = r;
<train> Battlecruiser    = b;   # requires fusion core

<train> wrath            = w;   # campaign only  # ???


## Nexus:

<train> probe            = e;

## Gateway:

<train> Zealot           = z;
<train> Stalker          = s;
<train> Sentry           = e;
<train> Dark templar     = d;
<train> High templar     = t;



### 
### Experiments:
### 

test set = {ctrl+f8};
test recall= {f8};

set tickle 1..12 = {ctrl+f$1};
reset tickle 1..12 = {shift+f$1};
tickle 1..12 = {f$1};
