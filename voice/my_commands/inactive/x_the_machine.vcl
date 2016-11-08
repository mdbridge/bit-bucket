###
### Voice commands for The Machine, restricted to X windows
###

include "switch.vch";
include "string.vch";

#<window_suffix> window	= SwitchTo($1$);
#focus <window_suffix>	= SwitchTo($1$) SendSystemKeys({win+shift+up}) OldWindow();
#return <window_suffix> = SwitchTo($1$) Wait(200) Window.Restore() OldWindow();


include gnu.vch;

set bold directories = Elisp("(setq cc-search-directories '"
    '("." "~/the_machine/bold/src" "~/the_machine/bold/include/bold" '
      '"/usr/include" "/usr/local/include/*")' ")");
