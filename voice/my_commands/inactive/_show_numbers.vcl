### 
### Voice commands for using ShowNumbers Plus!
### 

Control(action) := DdeExecute("ShowNumbersPlus", "CONTROL", "[$action]");


## 
## Numbering the taskbar:
## 

Show Taskbar Numbers = Control(SHOWNUMBERSTASKBAR);  
Hide Taskbar Numbers = Control(HIDENUMBERSTASKBAR);


Tray(number, action) := Control("DOACTIONTRAY=($number, $action)");

<action> := (Left=1 | Double=2 | Right=3 | Middle=4 | Control=5 | Shift=6 | Go=0);


Tray 1..99 = Tray($1, 7);

<action> Click Tray 1..99 = Tray($2, $1);



## 
## Numbering the controls of the current window:
## 

Show Numbers [Plus] = Control(SHOWNUMBERS);
Hide Numbers [Plus] = Control(HIDENUMBERS);

Do(number, action) := Control("DOACTIONSHOWNUMBERS=($number, $action)");

#<pick> := (pick=1 | show pick=1);
<pick> := (show pick=1);  # <<<>>>

         <pick> 0..9           = Do($2,     $1);
         <pick> 0..9 0..9      = Do($2$3,   $1);
         <pick> 0..9 0..9 0..9 = Do($2$3$4, $1);

<action> <pick> 0..9           = Do($3,     $1);
<action> <pick> 0..9 0..9      = Do($3$4,   $1);
<action> <pick> 0..9 0..9 0..9 = Do($3$4$5, $1);



## 
## Drag-and-drop
## 

Move Mouse To 1..99 = Do($1, 0);

Drag and drop 1..99 to 1..99 = Control("DOACTIONDRAGANDDROP=($1,$2)");

Drag 1..99 = Control("DOACTIONDRAG=($1)");
Drop 1..99 = Control("DOACTIONDROP=($1)");



## 
## MultiMon:
## 

right taskbar = SetMousePosition(0, 3838, 1061) ButtonClick(1, 1)
      	        Control(SHOWNUMBERS);
