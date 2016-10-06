Function SwitchToOrError(title As String, error As String) As Boolean
    SwitchToOrError = SwitchToWindow(title)

    If Not SwitchToOrError Then
        Beep()
        MsgBox error
    End If
End Function

Function SwitchToAndExit(title As String) As Boolean
    SwitchToAndExit = not SwitchToWindow(title)
End Function





'
' Attempt to switch to the top-level window having the exact title
' title; returns true if we succeed
' uniconifies window if it is uniconified
'
' version: 1
'
Function SwitchToWindow(title As String) As Boolean
	SwitchToWindow = False

	Dim window As Long
	window = FindWindowByTitle(0, title)
	If window = 0 Then
	    Exit Function
	End If

	If IsIconic(window) Then
	    If Not OpenIcon(window) Then
		Exit Function
	    End If
	End If
	
	If Not SetForegroundWindow(window) Then
	    Exit Function
	End If

	SwitchToWindow = True
End Function

'
' Win32 APIs:
'

Declare Function FindWindow Lib "user32" (ByVal lpClassName As String, ByVal lpWindowName As String) As Long

' This is a version of FindWindow that can be passed null (0) as its first argument:
Declare Function FindWindowByTitle Lib "user32" Alias "FindWindowA" (ByVal nullValueOnly As Long, ByVal lpWindowName As String) As Long

Declare Function IsIconic Lib "user32" (ByVal hWnd As Long) As Boolean

Declare Function OpenIcon Lib "user32" (ByVal hWnd As Long) As Boolean

Declare Function SetForegroundWindow Lib "user32" (ByVal hWnd As Long) As Boolean
