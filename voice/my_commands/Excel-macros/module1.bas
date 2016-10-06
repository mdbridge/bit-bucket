Attribute VB_Name = "Module1"
Sub move_chart()
Attribute move_chart.VB_Description = "Macro for relocating charts within a worksheet"
Attribute move_chart.VB_ProcData.VB_Invoke_Func = "q\n14"
'
' move_chart Macro
' Macro for relocating charts within a worksheet
'
' Keyboard Shortcut: Ctrl+q
'
    Dim current As String
    current = ActiveChart.Parent.Left & "," & ActiveChart.Parent.Top

    Dim coordinates As String
    coordinates = InputBox("Enter X,Y point to put top of current chart at (currently at " & current & ")")
    
    Dim x, y As String
    x = Left(coordinates, InStr(coordinates, ",") - 1)
    y = Mid(coordinates, InStr(coordinates, ",") + 1)
    
    With ActiveChart.Parent
        .Left = x
        .Top = y
    End With
End Sub
