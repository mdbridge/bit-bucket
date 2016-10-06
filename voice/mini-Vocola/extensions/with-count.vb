Function with_count(direction, target, repetition, template) As Boolean
	with_count = False

	If direction = "right" Then
		look_right = True
	Else
		look_right = False
	End If

	position = find_pattern(look_right, target, repetition, True)
	If position = 0 Then
		Beep
		Exit Function
	End If

	keys = Replace$(template, "#", Replace$(Str$(position-1), " ", ""))
	If position < 2 Then
		position = 2
	End If
	keys = Replace$(keys,     "%", Replace$(Str$(position-2), " ", ""))

	SendDragonKeys keys

	with_count = True
End Function


Function find_pattern(look_right, target, repetition, move_required) As Integer
    find_pattern = 0

    context = peek(look_right)
    If context = "" Then
	Exit Function
    End If

    context = add_word_boundaries(context)
    context = LCase$(context)

    If Not look_right Then
        context = StrReverse(context)
        target  = StrReverse(target)
    End If


    last = 0
    If move_required Then
        last = 1
    End If

    While repetition>0 
        position = InStr(last+1, context, target)
	If position = 0 Then
            Exit Function
        End If

        last = position
        repetition = repetition - 1	
    Wend


    before = Left$(context, last-1)
    If Not look_right Then
        before = before + target
    End If
    before = Replace$(before, Chr(1), "")
    before = Replace$(before, Chr(2), "")

    find_pattern = Len(before)+1
End Function

Function add_word_boundaries(text) As String
	add_word_boundaries = Left$(text, 1)
	in_word = is_word_character(add_word_boundaries)
	text = Mid$(text, 2)

	While Len(text) > 0
		next_character = Left$(text, 1)
		text = Mid$(text, 2)

		next_in_word = is_word_character(next_character)
		If Not in_word = next_in_word Then
			If in_word Then
				add_word_boundaries=add_word_boundaries + Chr(2)
			Else
				add_word_boundaries=add_word_boundaries + Chr(1)
			End If
		End If

		in_word = next_in_word
		add_word_boundaries = add_word_boundaries + next_character
	Wend
End Function

Function is_word_character(character) As Boolean
	If InStr("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'$%", character) = 0 Then
		is_word_character = False
	Else
		is_word_character = True
	End If
End Function



Function test(keys) As Boolean
	 result = peek_right(keys)
	 MsgBoxConfirm "'" + Replace$(result, Chr(13), "\n") + "'", 0, "peek_right " + keys
	 result = add_word_boundaries(result)
	 MsgBoxConfirm "'" + Replace$(result, Chr(13), "\n") + "'", 0, "peek_right " + keys

End Function


