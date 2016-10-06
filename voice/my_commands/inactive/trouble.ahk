#NoTrayIcon

Send, *
return

  MsgBox, 4144, SwitchTo, %1%|%2%|%3%|
return 

  Sleep, 5000
  FileAppend, foo,*
target_ID := ActivateActual()
if (target_ID <> 0) {
  Send Test
  Send, keys
  Sleep, 5000
  SendInput, keys1keys3keys1
  SendPlay, keys2
  SendEvent, keys3
}


ActivateActual() {
  WinGet, ids, List
  Loop, %ids%
  {
    i := ids%A_index%
    WinGet, exstyle, ExStyle, ahk_id %i%

    If (exstyle&0x8)
      continue
  
    WinActivate, ahk_id %i%
    return %i%
  }

  return 0
}
