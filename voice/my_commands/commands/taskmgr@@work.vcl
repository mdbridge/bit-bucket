### 
### Voice commands for Windows task manager (Windows 7 and earlier)
### 

<pane> := ( applications=1
          | processes=2
	  | services=3
   	  | performance=4
  	  | networking=5
  	  | users=6
  	  );

  # move focus to Applications pane tab:
PaneBar() := {ctrl+PgUp_5}{ctrl+home};

show <pane> = PaneBar() {ctrl+PgUp} {ctrl+PgDn_$1} {tab};
