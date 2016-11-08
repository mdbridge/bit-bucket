### 
### Voice commands for Chrome (version 54) developer tools
### 
### These commands use mouse clicks hardcoded for Chrome maximized on my home PC
### 

# "developer tools" toggles; it is defined in chrome.vcl


##
## These assume developer tools are open
##

  # these focus the tab
console	 = Mouse.Click(interior,1232,111);
elements = Mouse.Click(interior,1173,111);

inspect      [element] = {ctrl+shift+c};
inspect that [element] = {ctrl+shift+c} Mouse.Click();

  # aka, the main webpage
Left Pane = Mouse.Click(interior,1,521);


##
## These assume elements tab is open
##

styles		= Mouse.Click(interior,1704,136);
computed	= Mouse.Click(interior,1755,138);
computed bottom	= Mouse.Click(interior,1913,957);
event listeners = Mouse.Click(interior,1834,141);

#scroll right  = 



# look at shortcuts menu...  
