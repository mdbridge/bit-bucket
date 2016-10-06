### 
### Short commands for creating and managing Gnuemacs buffers/windows
### 

include "gnu.vch";

$set MaximumCommands 2;



Other Buffer            = {ctrl+x}b{enter};
kill  buffer            = {ctrl+x}k{enter};

other window            = {ctrl+x}o Empty();
SingleWindow            = {ctrl+x}1 Empty();

save file               = {ctrl+x}{ctrl+s};
save (message|messages) = S;
save mail = S;# <<<>>>


## 
## <<<>>>
## 

Single = {ctrl+x}1 Empty();
other  = {ctrl+x}o Empty();
second = {ctrl+x}b{enter};

menu   = Do(mdl-buffer-menu);
