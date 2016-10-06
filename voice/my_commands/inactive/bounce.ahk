;#
;# Same as SwitchTo, but if succeeds then sends $keys to the newly
;# activated window.  End $keys with "<<" to return afterwards to the
;# original window.
;#
;#SwitchToKeys(name, keys) := AutoHotkey2(bounce, $name, $keys);
;OldSwitchToKeys(name, keys) := AutoHotkey2(bounce, $name, $keys);
;
;Mark makes      trouble = OldSwitchToKeys("startup xterm", "hit me!{enter}" <<) {ctrl+u}0{ctrl+l};


#NoTrayIcon

;;; 
;;; Switch to a given window or produce an error dialog box which fades
;;; after a few seconds.
;;; 
;;; Afterwards, send optional keys to newly activated window.
;;; 
;;; Use "<<" at end of keys to activate the original active window.
;;; 

;#include C:\Users\Mark\Documents\AutoHotkey\routines.ahk


Target = %1%
Keys   = %2%


ifInString, Keys, <<
{
  Actual_ID := ActualID()
}


if (SwitchTo(Target) = 0) {
  MsgBox, 4144, SwitchTo, %Target%: no such window found, 3
  ExitApp, 1
}


Bounce = 0
ifInString, Keys, <<
{
  Bounce = 1
  StringGetPos, pos, Keys, <<
  StringLeft, Keys, Keys, pos
}

Keys := ConvertKeys(Keys)
Send, %Keys%

if (Bounce) {
  WinActivate, ahk_id %Actual_ID%
}

ExitApp, 0


; ===================================================================

;;; 
;;; AutoHotkey functions/procedures for dealing with Dragon.
;;; 
;;; Version 0.1
;;; 

;; 
;; Return window ID of foremost window that does not have the topmost
;; property or 0 if none.
;; 
;; (Kludge for finding window that user thinks is actually active; does
;;  not work for topmost applications like DragonBar.)
;; 
ActualID() {
  WinGet, ids, List
  Loop, %ids%
  {
    i := ids%A_index%
    WinGet, exstyle, ExStyle, ahk_id %i%

    If (exstyle&0x8)   ; does the window have the topmost property?
      continue
  
    return %i%
  }

  return 0
}


;; 
;; Convert Keys from Dragon NaturallySpeaking SendDragonKeys format to 
;; AutoHotkey format:
;; 
;;  (probably not perfect)
;;
ConvertKeys(Keys) {
  StringReplace, Keys, Keys, +, {+}, 1
  StringReplace, Keys, Keys, ^, {^}, 1
  StringReplace, Keys, Keys, !, {!}, 1
  
  StringReplace, Keys, Keys, {shift{+}, +{, 1
  StringReplace, Keys, Keys, {ctrl{+}, ^{, 1
  StringReplace, Keys, Keys, {alt{+}, !{, 1
  
  StringReplace, Keys, Keys, {shift{+}, +{, 1
  StringReplace, Keys, Keys, {ctrl{+}, ^{, 1
  StringReplace, Keys, Keys, {alt{+}, !{, 1
  
  StringReplace, Keys, Keys, {shift{+}, +{, 1
  StringReplace, Keys, Keys, {ctrl{+}, ^{, 1
  StringReplace, Keys, Keys, {alt{+}, !{, 1

  return Keys
}


;; 
;; Extract matching mode indicator if any from window title target:
;; 
;;   Returns title w/o optional mode indicator prefix, sets
;;   SetTitleMatchMode operatively, and sets global variable Mode to
;;   the name of the chosen mode.
;; 
ExtractMatchingMode(Target) {
  global Mode := "Prefix"
  SetTitleMatchMode, 1
  
  if (SubStr(Target,1,2) = "p)") {
    Mode = Prefix
    Target := SubStr(Target,3)
    SetTitleMatchMode, 1
  }
  if (SubStr(Target,1,2) = "c)") {
    Mode = Contains
    Target := SubStr(Target,3)
    SetTitleMatchMode, 2
  }
  if (SubStr(Target,1,2) = "R)") {
    Mode = RegEx
    Target := SubStr(Target,3)
    SetTitleMatchMode, RegEx
  }

  return Target
}


;; 
;; Search for window matching %Target%, ignoring DNS results box:
;; 
FindRealWindow(Target) {
  WinGet, ids, List, %Target%
  Loop, %ids%
  {
    i := ids%A_index%
    WinGetTitle, title,                ahk_id %i%
    WinGetClass, class,                ahk_id %i%
    WinGet,      process, ProcessName, ahk_id %i%
  
    If (class = "#32770" and process = "natspeak.exe" and title <> "Messages from Python Macros")
      continue
  
    return i
  }

  return 0
}


;; 
;; Switch to a given window (ignoring the DNS results box) then return its 
;; Windows ID or 0 if the given window was not found.
;; 
SwitchTo(Target) {
  Target := ExtractMatchingMode(Target)
  Target_ID := FindRealWindow(Target)
  if (Target_ID) {
    WinActivate, ahk_id %Target_ID%
  }

  return Target_ID
}


;; 
;; Debugging routine: pop up a message for 3 seconds
;; 
PopUp(Message) {
  MsgBox, 4144, PopUp, %Message%, 3
}
