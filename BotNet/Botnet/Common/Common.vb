Imports System.IO

Public Module Common
    Public ApplicationsPath As String
    Public DatabasePath As String
    Public AccountsPath As String
    Public DBAccountsPath As String
    Public Const DBFolder As String = "database\"
    Public Const DBAccounts As String = "db\"
    Public Const UserFolder As String = "users\"
    Public Const FileHub As String = "hub.txt"
    Public Const FileAccounts As String = "account.txt"
    Public Const AccountPassDat As String = "pass.txt"
    Public Const BanTimer As String = "bantimer.txt"
    Public Const DEFAULT_HUB_ID As String = "Public"

    Public Enum STRING_LENGTHS
        ACCOUNT_LENGTH = 16
        PASSWORD_LENGTH = 96
    End Enum

#Region "File / Folder IO"
    Public Function GetDatabaseFlags(ByVal database As String, ByVal username As String)
        'Check if user exists in the db in question if not create the file since they have access.
        Dim strFile As String = DBAccountsPath & database & "\" & username & ".txt"
        If Not File.Exists(strFile) Then
            If CreateFile(strFile) Then
                WriteLineToFile(strFile, "C") 'A here means read only
            End If
        End If
        Dim sr As StreamReader = New StreamReader(strFile)
        Dim outBuf As String = sr.ReadToEnd()
        sr.Close()
        sr = Nothing
        If outBuf = "" Then
            Return 4 'Restricted
        End If
        Return GetFlagFromString(outBuf)
    End Function
    Public Function GetDatabasePass(ByVal database As String) As String
        Dim strFolder As String = DBAccountsPath & database & "\pass\"
        'first of all does the database in question exist.
        If Not Directory.Exists(strFolder) Then
            Return "" 'If the db dosent exist 
        End If
        Dim sr As StreamReader = New StreamReader(strFolder & AccountPassDat)
        Dim outBuf As String = sr.ReadToEnd()
        sr.Close()
        sr = Nothing
        Return outBuf
    End Function
    Public Function GetAccountFlags(ByVal strPath As String) As UInt32
        Dim sr As StreamReader = New StreamReader(strPath & "\" & FileAccounts)
        Dim outBuf As String = sr.ReadToEnd()
        sr.Close()
        sr = Nothing
        If outBuf = "" Then
            Return 0
        End If
        Return GetFlagFromString(outBuf)
    End Function
    Private Sub DelFile(ByVal strPath As String)
        Try
            File.Delete(strPath)
        Catch errr As IOException
            Return 'Failed delete
        End Try
    End Sub
    Public Function NewPassword(ByVal strPath As String, ByVal NewPass As String) As String
        DelFile(strPath & AccountPassDat)
        If Not File.Exists(strPath & AccountPassDat) Then
            If Not CreateFile(strPath & AccountPassDat) Then
                Return GetPassword(strPath) 'File did not delete.
            End If
        End If
        WriteLineToFile(strPath & AccountPassDat, NewPass)
        Return GetPassword(strPath) 'need to return the new value to check that the password matches said new password. [yes i know it can be done here...]
    End Function
    Public Function GetPassword(ByVal strPath As String) As String
        Dim sr As StreamReader = New StreamReader(strPath & AccountPassDat)
        Dim outBuf As String = sr.ReadToEnd()
        sr.Close()
        sr = Nothing
        Return outBuf
    End Function
    Private Sub WriteLineToFile(ByVal strPath As String, ByVal message As String)
        Dim xfile As System.IO.StreamWriter
        xfile = My.Computer.FileSystem.OpenTextFileWriter(strPath, True)

        xfile.Write(message)
        xfile.Close()
        xfile = Nothing
    End Sub
    Public Function CreateAccount(ByVal strPath As String, ByVal password As String) As Boolean
        'check dir, do we need to create it?
        If Not Directory.Exists(strPath) Then
            If Not CreateFolder(strPath) Then
                Return False 'couldent create the folder.....
            End If
        End If
        'check all files needed for this account.
        If Not File.Exists(strPath & FileAccounts) Then
            If Not CreateFile(strPath & FileAccounts) Then
                Return False 'couldent create the account file.....
            End If
        End If
        If Not File.Exists(strPath & AccountPassDat) Then
            If Not CreateFile(strPath & AccountPassDat) Then
                Return False 'couldent create the account file.....
            End If
        End If
        If Not File.Exists(strPath & BanTimer) Then
            If Not CreateFile(strPath & BanTimer) Then
                Return False 'couldent create the account file.....
            End If
        End If
        WriteLineToFile(strPath & AccountPassDat, password)
        Return True
    End Function

    Public Function CreateFile(ByVal strFile As String) As Boolean
        Try
            If Not File.Exists(strFile) Then
                Dim fs As FileStream = File.Create(strFile)            'Create the file.
                fs.Close()
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

End Module
