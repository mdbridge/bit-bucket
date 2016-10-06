### 
### Voice commands for Lync 2013 (office communicator)
### Says "Skype for business" on its windows now
### 
### See also _lync.vcl for global commands for handling incoming calls
### 

include "email_addresses.vch";

<people> := ( 
	      Harumi = "Kuno, Harumi"
            | Hotel  = "Kimura, Hideaki"
            | Joe    = "Tucek, Joseph" 
            | Kim    = "Keeton, Kimberly"
            | Rob    = "Schreiber, Robert"
            );

Contact(name) := {ctrl+1} $name Wait(500) {tab_2}{enter};

contact person		    = {ctrl+1};
contact clipboard	    = Contact({ctrl+v});
contact <people>	    = Contact($1);
contact [work] <work_email> = Contact($1);

(contact|call) IT           = Contact(281-377-2301);  # global service desk
Brad employee number	    = 20064438;


IM area		    = {ctrl+shift+m};

(over | ping)	    = {ctrl+shift+m}$1{enter};

send    <_anything> =                     {ctrl+shift+m} $1 {enter};
prepare <_anything> = {ctrl+a}{backspace} {ctrl+shift+m} $1;

unmute = Keys.SendInput({win+f4});



hang up = {ctrl+enter};

conversation tab = {ctrl+1};
phone tab	 = {ctrl+3};
