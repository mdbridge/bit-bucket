Attribute VB_Name = "emacs_words"
'
' Bugs: does not treat end of buffer as a word start and end boundary
'

Sub startWord()              ' bind to {ctrl+shift+s}
'
' startWord Macro
'
    Dim wordChar As String
    wordChar = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'$%" + Chr(146)

    Dim oldEnd As Long
    oldEnd = Selection.End

    If Selection.StartIsActive Then
        Selection.MoveStartWhile Cset:=wordChar
        ' the below does not work correctly with the end of the buffer:
        Selection.MoveStartUntil Cset:=wordChar
        Selection.StartIsActive = True
        
        If Selection.End <> oldEnd Then
            Selection.Start = oldEnd
            Selection.StartIsActive = False
        End If

    Else
        Selection.MoveEndWhile Cset:=wordChar
        ' the below does not work correctly with the end of the buffer:
        Selection.MoveEndUntil Cset:=wordChar
    End If

End Sub

Sub endWord()               ' bind to {ctrl+shift+e}
'
' endWord Macro
'
    Dim wordChar As String
    wordChar = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'$%" + Chr(146)

    Dim oldEnd As Long
    oldEnd = Selection.End

    If Selection.StartIsActive Then
        Selection.MoveStartUntil Cset:=wordChar
        ' the below does not work correctly with the end of the buffer:
        Selection.MoveStartWhile Cset:=wordChar
        Selection.StartIsActive = True
        
        If Selection.End <> oldEnd Then
            Selection.Start = oldEnd
            Selection.StartIsActive = False
        End If

    Else
        Selection.MoveEndUntil Cset:=wordChar
        ' the below does not work correctly with the end of the buffer:
        Selection.MoveEndWhile Cset:=wordChar
    End If

End Sub

Sub backWord()              ' bind to {ctrl+shift+b}
'
' backWord Macro
'
    Dim wordChar As String
    wordChar = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'$%" + Chr(146)

    Dim oldStart As Long
    oldStart = Selection.Start

    If Selection.StartIsActive Then
        Selection.MoveStartUntil Cset:=wordChar, Count:=wdBackward
        ' the below does not work correctly with the end of the buffer:
        Selection.MoveStartWhile Cset:=wordChar, Count:=wdBackward
        Selection.StartIsActive = True
    Else

        Selection.MoveEndUntil Cset:=wordChar, Count:=wdBackward
        ' the below does not work correctly with the end of the buffer:
        Selection.MoveEndWhile Cset:=wordChar, Count:=wdBackward

        If Selection.Start <> oldStart Then
            Selection.End = oldStart
            Selection.StartIsActive = True
        End If
    End If

End Sub

