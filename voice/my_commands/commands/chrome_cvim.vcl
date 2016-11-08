###
### Voice commands for Chrome using the cVim Chrome extension
###
###
### Requires cVim chrome extension
###
### Configure via:
###
###    green V icon->settings:
###       add to cVimrc from ~/admin/cvimrc
###     then save
###

## 
## Clicking links/gizmos:
## 

  # from CbV Chrome extension:
Blur() := "{ctrl+shift+,}" Wait(100);

Vim(command) := @ $command;

SetH(value)  := Variable.Set(chrome:hint,  $value);
H()	     := Variable.Get(chrome:hint,  "");
Action(code) := When($code,{esc} Vim($code) Wait(300),"") H(); 

Focus() := Action(G);



show vim hints = Blur() Vim(f);

vim <pick> 0..9 [0..9 [0..9 [0..9]]] = SetH($2$3$4$5) $1;

<pick> := (         pick = Action("")
          | proceed	 = Action("")
          | hit	    pick = Action("")

          | push    pick = Action(F)     # stay but open new tab w/ link
          | tab	    pick = Action(T)
          | window  pick = Action(W)

          | go	    pick = Focus()
          | menu    pick = Focus() Wait(200) {shift+f10}
	  | HP	    pick = Focus() Wait(500) mark.lillibridge@hpe.com{tab}

          | hover   pick = Action(q)
          | unhover pick = Action(Q)
);
