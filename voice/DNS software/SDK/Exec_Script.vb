Module Exec_Script

    Function Main(ByVal cmdArgs() As String) As Integer
        Dim Script As String
        Script = ""
        If cmdArgs.Length > 0 Then
            For argNum As Integer = 0 To UBound(cmdArgs, 1)
                Script = Script & vbCrLf & cmdArgs(argNum)
            Next argNum
        End If

        Dim control As DNSTools.DgnVoiceCmd
        control = New DNSTools.DgnVoiceCmd()

        control.Register("")

        Dim errLine As Long
        Dim errString As String
        errLine = control.CheckScript(Script, errString)
        If errLine <> 0 Then
            MsgBox("Error in script at command " & errLine - 1 & ": " & errString)
        Else
            control.ExecuteScript(Script, 0)
        End If

        control.UnRegister()

        Return errLine
    End Function

End Module
