' 
' Peek functions:
' 
'   Requires: not in middle of defining a text selection
'   Side effect: these functions destroy the clipboard.
' 

  ' look_right: boolean
  '
  ' returns "" on error
  '
  ' may move point or insert a few characters if application doesn't
  ' support Windows keys properly
  '
Function peek(look_right) As String
    If look_right Then
        peek = peek_right("{shift+end}{shift+right}{shift+end}")
    Else
        peek = peek_left("{shift+home}{shift+left}{shift+home}")
    End If
End Function






Function peek_left(keys) As String
	Clipboard("")
	SendDragonKeys "(){left}" + keys + "{ctrl+c}"
	selection = Clipboard()
	If selection = "" Then
	        ' something went wrong, probably the copy;
		' no way for us to safely remove the () now, alas...
		peek_left = ""
		Exit Function
	End If

	selection = Replace$(selection, Chr(10), "")

	' Microsoft Word adds a carriage return to end of clipboard if
	' not already present:
	If Right(selection, 1) = Chr(13) Then
		' can safely remove carriage return because selection should 
		' end with "(":
		selection = Left(selection, Len(selection)-1)
	End If

	' undo selection, leaving point in original location:
	SendDragonKeys "{shift+right " + Len(selection) + "}{backspace}{Del}"

	peek_left = Left(selection, Len(selection)-1)  ' discard "("
End Function


Function peek_right(keys) As String
	Clipboard("")
	SendDragonKeys "()){left 2}" + keys + "{ctrl+c}"
	selection = Clipboard()
	If selection = "" Then
	        ' something went wrong, probably the copy;
		' no way for us to safely remove the ()) now, alas...
		peek_right = ""
		Exit Function
	End If

	selection = Replace$(selection, Chr(10), "")

	' Microsoft Word adds a carriage return to end of clipboard if
	' not already present:
	If Right(selection, 1) = Chr(13) Then
		selection = Left(selection, Len(selection)-1)
	End If

	' undo selection, leaving point in original location:
	SendDragonKeys "{shift+left " + Len(selection) + "}{Del 2}{backspace}"

	peek_right = Right(selection, Len(selection)-2)
End Function















'	this should work for anything except (in rare cases) command browser
'	  (can fail on lines with more than eight spaces or tabs at the beginning)
'	requires a modifiable field, leaves spaces if field doesn't support {ctrl+c}


Function test1(keys) As Boolean
	 MsgBoxConfirm "'" + peek_left(keys) + "'", 0, "peek_left " + keys
End Function

Function test2(keys) As Boolean
	 MsgBoxConfirm "'" + Replace$(peek_right(keys), Chr(13), "\n") + "'", 0, "peek_right " + keys
End Function


