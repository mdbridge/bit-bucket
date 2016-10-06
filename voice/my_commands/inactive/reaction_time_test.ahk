;;; 
;;; Test of human reaction speeds via foot switch
;;; 
;;;   normal keyboard appears to be about 40 ms once started repeating
;;;   that and below, I can get within several characters
;;; 
;;;   web claims 160 ms good college student reaction time...
;;; 

#IfWinActive emacs

LButton::
  SetTimer, heldDown, 50
  return

LButton up::
  SetTimer, heldDown, Off
  return

heldDown:
  Send ^f
  return
