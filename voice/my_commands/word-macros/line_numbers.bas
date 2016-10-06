Attribute VB_Name = "line_numbers"
Sub GotoLine()                 ' bind to {ctrl+shift+g}
'
' GotoLine Macro
'
    Dim targetLine, currentLine As Integer
    Dim target As String
    
    target = InputBox("Enter line on current page to go to")
    targetLine = Val(target)
    
    currentLine = Selection.Range.Information(wdFirstCharacterLineNumber)
       
    If targetLine < currentLine Then
        Selection.MoveUp Unit:=wdLine, Count:=(currentLine - targetLine)
    End If
        
    If targetLine > currentLine Then
        Selection.MoveDown Unit:=wdLine, Count:=-(currentLine - targetLine)
    End If

    Selection.HomeKey Unit:=wdLine
End Sub

