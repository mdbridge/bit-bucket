###
### Website specific commands for Chrome (47)
###

include "chrome.vch";


GoField(word) := {ctrl+f} Wait(100) $word{enter} Wait(100) {esc}{tab};


##
## Commands for livejournal:
##

  # after "Livejournal login page":
login to live journal =
    GoField("username")
