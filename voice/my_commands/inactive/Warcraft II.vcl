### 
### Commands for Warcraft II
### 

$set MaximumCommands 3;



### 
### SmartNav commands:
### 

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

<modifier> touch = ModTouch($1);


      lasso = ModTouch(8);
giant lasso = ModTouch(9);



###
### Overall gameplay:
### 

# command mode (on|off)

game menu    = {alt+m};
pause game   = {alt+m}; 
save  game   = {alt+s};

normal speed = SendSystemKeys( Repeat(10, -)               );
speed 1..10  = SendSystemKeys( Repeat(10, -) Repeat($1, +) );


# tab:   toggle mini map information



### 
### Changing current viewpoint:
### 

center = {alt+c};

# space: center on last transmission


  # these scroll the visible window:
<direction> := (left|right|soar=up|down); 

<direction> 1..20 = SendSystemKeys({$1_$2});


## 
## Map points:
## 

#
# built-in ones:
#
set map 1..3 = {shift+f Eval($1+1) };
    map 1..3 = {      f Eval($1+1) };


# 
# extended ones:
# 
GoExtended(n) := Mouse.Set(Variable.Get(map_$n)) ButtonClick(1, 1)
	         Variable.Set(last_map, Variable.Get(map_$n));                 

manually set map 4..20 = Variable.Set(map_$1, Mouse.Get()) GoExtended($1);

set map 4..20 = WithPause(Variable.Set(map_$1, Mouse.Get()) GoExtended($1));
    map 4..20 = WithPause(                                  GoExtended($1));



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
           | rally point=__r
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
set <place>      = SetPlace($1, 1 );
set <place> 1..5 = SetPlace($1, $2);

    <place>      =  GoPlace($1, 1 );
    <place> 1..5 =  GoPlace($1, $2);



### 
### Selecting units/groups of units:
### 

<group> := (group | unit);


you all             = ButtonClick(1, 2);    # select all (up to 9) of type

recall <group>      = ModTouch(3);


# shift touch: toggle units membership in selected group
# alt   touch: recall group unit was last in

# can point to a unit's portrait when selected


set <group> 0..9 = {ctrl+$2};
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
### Non-building/magic spell unit commands:
### 

there               =          ButtonClick(2, 1);
here                = Wait(50) ButtonClick(1, 1);


move                = m;
patrol              = p;                 # fighting units
stop                = s;
stand ground        = t;                 # fighting units

attack              = a;
attack ground       = g;                 # catapults, battleships
charge              = a Wait(50) ButtonClick(1,1);


repair              = r;                 # Peons
harvest             = h;                 # Peons
return [with] goods = g;                 # Peons
haul oil            = h;                 # oil tankers

unload [transport]  = u;                 # transports

demolish            = SendSystemKeys(d); # Goblin Sappers/demolition squad



### 
### Building buildings:
### 

<building> := ( great hall=h | town hall = h | hall=h 
              | farm=f 
              | barracks=b 
              | tower=t 
              | lumber mill=l 
              | blacksmith=s 
              );

<advanced_building> := ( shipyard=s                        # requires mill
                       | foundry=f                         # requires shipyard
                       | oil refinery=r | refinery=r       # requires shipyard

		       	   # below require keep/stronghold:
                       | goblin alchemist=a | alchemist=a  
                       | ogre mound=o | mound=o 

		       | stables=a
		       | Gnomish inventor=i | inventor=i

		           # below require castle/Fortress:
                       | dragon roost=d | roost=d          
                       | altar of storms=l | altar=l       
                       | temple of the damned=t | temple=t 

		       | Gryphon aviary=g | aviary=g
		       | mage tower=m
		       | church=c
                       );

# upgrade to (stronghold=s|keep=k): requires barracks
# upgrade to (fortress=f|castle=c): requires barracks, blacksmith, mill, mound


## Peons/peasants:

build          structure       = b;
build advanced structure       = v;

build <building>                 = b Wait(200) $1;
build <advanced_building>        = v Wait(200) $1;


## Oil tanker-only commands:

(build|drill) oil (platform|rig) = b;



### 
### Training units:
### 

<train> := ( build | train );


## (Great|Town) hall commands:

<train> (peon|peasant)               = p;


## Barrack's commands:

<train> (grunt=g|footman=f)          = gf;
<train> (ax thrower|archer)          = a;   # requires a lumber mill
<train> (catapult=c|ballista=b)      = cb;  # requires a blacksmith

<train> (ogre=o|knight=kp)           = $2;  # requires a ogre mound/stables
<train> paladin                      = pk;  # requires above + church training


## Shipyard commands:

<train> oil tanker                   = o;
<train> destroyer                    = d; 
<train> transport                    = t;   # requires foundry
<train> (juggernaut=j|battleship=b)  = jb;  # requires foundry

<train> [giant] turtle               = t;   # requires alchemist, ?
<train> [gnomish] (sub|submarine)    = s;   # requires ?


## Goblin alchemist/gnomish inventor:

<train> [goblin] zeppelin            = z; 
<train> [goblin] sapper              = s;   # requires ?

<train> flying machine               = f;
<train> [Dwarven] [demolition] squad = d;


## Mage tower/Temple of the damned:

<train> mage                         = t;
<train> death knight                 = t;   # requires a ??


## Dragon roost/Gryphon aviary:

<train> [Gryphon] rider              = g;
<train> dragon                       = ?;



### 
### Magic commands:
### 

## 
## Paladin-only commands:
## 

[holy] vision         = v;
(exorcism | exorcise) = e;
heal                  = h;

cure = ButtonClick(1, 1) Wait(50) h;

healing dance = Repeat(50, ButtonClick(1, 1) Wait(100) h);



## 
## Mage-only commands:
## 

fireball     = f;
invisibility = i;
polymorph    = p;
blizzard     = b;
flame shield = l;
slow         = o;









## 
## Ogre-Mage-only commands:
## 

<spell> := ( bloodlust=b | runes=r | eye=e | eye of Kilrogg=e );

cast <spell> = $1; 

bloodlust = ButtonClick(1, 1) Wait(50) b;


## 
## Death knight-only commands:
## 

<magic> := ( death coil=c | coil=c
           | death and decay=d | death=d
           | haste=h | whirlwind=w
           | unholy armor=u | armor=u
           | raise the dead=r | raise=r
           );

cast <magic> = $1; 
