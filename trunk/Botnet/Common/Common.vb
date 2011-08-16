Imports System.IO

Public Module Common
    Public ApplicationsPath As String
    Public Const DBFolder As String = "database\"
    Public Const FileHub As String = "hub.txt"
    Public Const FileAccounts As String = "accounts.txt"
    Public Const DEFAULT_HUB_ID As String = "Public"

    Public Function ValidateHubAccount(ByVal HUB_ID_VALUE As String, ByVal HUB_PASSWORD As String) As Boolean
        Dim HubLocation As String = ApplicationsPath & DBFolder & FileHub
        If Not File.Exists(HubLocation) Then Return False 'if the file dosent exist then fail
        Dim reader As StreamReader = New StreamReader(HubLocation)
        Dim TempBuffer() As String
        Dim i As UInteger
        Dim ValuesRequired() As String

        TempBuffer = reader.ReadToEnd.Replace(vbCrLf, Chr(0).ToString).Split(Chr(0))
        reader.Close()
        If Not TempBuffer.Length >= 3 Then Return False 'no data in the file
        For i = 0 To (TempBuffer.Length - 1)
            ValuesRequired = TempBuffer(i).Split(CChar("%"))
            '0=name, 1=password
            If ValuesRequired.Length >= 2 Then
                If ValuesRequired(0).ToLower = HUB_ID_VALUE.ToLower Then
                    If ValuesRequired(1).ToLower = HUB_PASSWORD.ToLower Then
                        Return True
                    Else
                        If ValuesRequired(1) = "*" Then
                            Return True
                        Else
                            Return False
                        End If
                    End If
                End If
            End If
        Next
        Return False 'if it gets here the hubid didnt exist in the folder
    End Function
End Module
