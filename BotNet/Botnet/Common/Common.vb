Imports System.IO

Public Module Common
    Public ApplicationsPath As String
    Public DatabasePath As String
    Public AccountsPath As String
    Public Const DBFolder As String = "database\"
    Public Const UserFolder As String = "users\"
    Public Const FileHub As String = "hub.txt"
    Public Const FileAccounts As String = "account.txt"
    Public Const DEFAULT_HUB_ID As String = "Public"

#Region "File / Folder IO"
    Public Function CreateFile(ByVal strFile As String) As Boolean
        Try
            If Not File.Exists(strFile) Then
                File.Create(strFile)            'Create the file.
                Return File.Exists(strFile)     'Check if the file created or not.
            End If
            Return False                        'File already exists
        Catch ex As Exception
            Return False                        'Error creating file
        End Try
        Return False                            'Creation Failed some other reason (Should not get here)
    End Function

    Public Function CreateFolder(ByVal strFolder As String) As Boolean
        Try
            If Not Directory.Exists(strFolder) Then
                Directory.CreateDirectory(strFolder)    'Create the folder
                Return Directory.Exists(strFolder)      'Now check if it created the folder or not and return.
            End If
            Return False                                'Folder already exists
        Catch ex As Exception
            Return False                                'Error creating folder
        End Try
        Return False                                    'Creation Failed some other reason (Should not get here)
    End Function


#End Region

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

    Public Enum PROTOCOL_ID
        BOTNET = 0
        BNET = 1

    End Enum
End Module
