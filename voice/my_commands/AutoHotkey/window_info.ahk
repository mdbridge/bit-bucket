;#space::

results := ""

;WinGet, i, ID, A
WinGetTitle, title, A
WinGetClass, class, A
WinGet, process, ProcessName, A
WinGet, exstyle, ExStyle, A

i := WinExist("A")


clipboard := title

results := results . "title | class | process | exstyle | ID`n`n`n"

results := results . "Active window: " . title . "|" . class . "|" . process . "|" . exstyle . "|" . i . "`n`n"


;DetectHiddenWindows, On
WinGet, ids, List, %Target%
Loop, %ids%
{
  i := ids%A_index%
  WinGetTitle, title, ahk_id %i%
  WinGetClass, class, ahk_id %i%
  WinGet, process, ProcessName, ahk_id %i%
  WinGet, exstyle, ExStyle, ahk_id %i%

  results := results . title . "|" . class . "|" . process . "|" . exstyle . "|" . i . "`n"
}

MsgBox, 0, Window info, %results%
