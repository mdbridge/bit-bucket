Function Split(Text, separator, index) As String
	start = 1
	While index > 0
		start = InStr(start, Text, separator) + 1
		If start = 1 Then
			Split = ""
			Exit Function
		End If
		index = index - 1
	Wend

	sep = InStr(start, Text, separator)
	If sep = 0 Then
		sep = Len(Text)+1
	End If

	Split = Mid(Text, start, sep - start)
End Function
