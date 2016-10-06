Attribute VB_Name = "leap"
Sub leap()                 ' bind to {ctrl+shift+l}
'
' leap Macro
'
    Dim target As String
    target = InputBox("Enter string to leap to")
    If target = "" Then
        target = Selection.Find.Text
    Else
        Selection.Find.Text = target
    End If
    
    Dim point As Range
    Dim mark As Long
    Set point = Selection.Range
    If Selection.StartIsActive Then
        point.Collapse (wdCollapseStart)
        mark = Selection.End
    Else
        point.Collapse (wdCollapseEnd)
        mark = Selection.Start
    End If

    
    point.Move Unit:=wdCharacter, Count:=1
    
    point.Find.ClearFormatting
    With point.Find
       .Text = target
       .Replacement.Text = ""
       .Forward = True
       .Wrap = wdFindStop     ' wdFindContinue
       .Format = False
       .MatchCase = False
       .MatchWholeWord = False
       .MatchWildcards = False
       .MatchSoundsLike = False
       .MatchAllWordForms = False
    End With
    If point.Find.Execute Then
        If point.Start > mark Then
            Selection.SetRange Start:=mark, End:=point.Start
            Selection.StartIsActive = False
        Else
            Selection.SetRange Start:=point.Start, End:=mark
            Selection.StartIsActive = True
        End If
    Else
        Beep
    End If

End Sub

Sub retreat()              ' bind to {ctrl+shift+r}
'
' retreat Macro
'
    Dim target As String
    target = InputBox("Enter string to retreat to")
    If target = "" Then
        target = Selection.Find.Text
    Else
        Selection.Find.Text = target
    End If
    
    Dim point As Range
    Dim mark As Long
    Set point = Selection.Range
    If Selection.StartIsActive Then
        point.Collapse (wdCollapseStart)
        mark = Selection.End
    Else
        point.Collapse (wdCollapseEnd)
        mark = Selection.Start
    End If
    
    point.Find.ClearFormatting
    With point.Find
       .Text = target
       .Replacement.Text = ""
       .Forward = False
       .Wrap = wdFindStop     ' wdFindContinue
       .Format = False
       .MatchCase = False
       .MatchWholeWord = False
       .MatchWildcards = False
       .MatchSoundsLike = False
       .MatchAllWordForms = False
    End With
    If point.Find.Execute Then
        If point.Start > mark Then
            Selection.SetRange Start:=mark, End:=point.Start
            Selection.StartIsActive = False
        Else
            Selection.SetRange Start:=point.Start, End:=mark
            Selection.StartIsActive = True
        End If
    Else
        Beep
    End If
    
End Sub
