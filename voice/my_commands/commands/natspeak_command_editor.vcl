###
### Voice commands for Naturally Speaking's command browser's command
### editor window:
###

MyCommands Editor:


  # bring up and focus on list of this command's lists for viewing/editing
list editor = {alt+m}{tab}{tab}{down};


  # enter expression for ListVarN's written part only, with optional
  # spoken part:
magic variable 1..10 =
	'Left$(ListVar$1,InStr(ListVar$1+"\","\")-1)';
