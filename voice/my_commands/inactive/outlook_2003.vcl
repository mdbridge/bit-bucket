###
### Voice commands for Microsoft Outlook 2003 (work version)
###

include "string.vch";



##
## Starting to compose a message:
##

# 
# Standard commands to compose and forward messages:
# 

  # version for work:
Compose(addresses) :=
    {alt+f}wm
	{tab}{tab} mark.lillibridge@hp.com {shift+tab_2}
	Replace($addresses, ",", "; ") {tab} {alt+j};

  # version for home:
Compose2(addresses) :=
    {alt+f}wm
	{tab} mark.lillibridge@hp.com {shift+tab}
	Replace($addresses, ",", "; ") {tab 2};

Forward(addresses) := 
	{alt+w} 
	{tab}{tab} mark.lillibridge@hp.com {shift+tab_2}
	Replace($addresses, ",", "; ") {tab} {alt+j} {tab};

include "mail.vch";


# 
# Other recipients:
# 

# user-specified:

compose message =
	{alt+f}wm
	{tab}{tab}mark.lillibridge@hp.com{shift+tab_2};

forward message =
	{alt+w} 
	{tab}{tab}mark.lillibridge@hp.com{shift+tab_2};


## 
## Switching to given folders:
## 

<folder> := ( attachments | invitations 
	    | tube results = "\\Tube results\tube results"
	    | Embarcadero results = "\\Tube results\Embarcadero results"
	    );

  # this crashes Outlook without the wait...
GoFolder(name) := {ctrl+y}{tab_3} $name Wait(500) {enter};

show <folder> = GoFolder($1);
