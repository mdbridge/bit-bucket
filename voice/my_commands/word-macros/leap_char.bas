Attribute VB_Name = "leap_char"
Sub leapChar()                 ' bind to {ctrl+shift+l}
'
' leapChar Macro
'
    Dim target As String
    target = InputBox("Enter character to leap to")

    Selection.MoveEnd Unit:=wdCharacter, Count:=1
    Selection.MoveEndUntil Cset:=target
    Selection.StartIsActive = False
End Sub

Sub retreatChar()              ' bind to {ctrl+shift+r}
'
' retreatChar Macro
'
    Dim target As String
    target = InputBox("Enter character to retreat to")

    Selection.MoveStartUntil Cset:=target, Count:=wdBackward
    Selection.MoveStart Unit:=wdCharacter, Count:=-1
    Selection.StartIsActive = True
End Sub
