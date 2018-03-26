Imports System.Threading
Imports System.Net.Sockets
Imports System.Xml.Serialization
Imports System.IO
Imports System.Net
Imports System.Reflection
Imports System.Runtime.CompilerServices
Imports System.ComponentModel

Public Class Form1

    Private Function InitFiles() As Boolean
        'Set the strings
        ApplicationsPath = Application.StartupPath & "\"

        'Create folder if not existing Database folder
        DatabasePath = ApplicationsPath & DBFolder
        If Not Directory.Exists(DatabasePath) Then
            'create the folder dosent exist..
            If Not CreateFolder(DatabasePath) Then
                Return False 'creation failed
            End If
        End If
        'Create folder if not existing users folder
        AccountsPath = DatabasePath & UserFolder
        If Not Directory.Exists(AccountsPath) Then
            'create the folder dosent exist..
            If Not CreateFolder(AccountsPath) Then
                Return False 'creation failed
            End If
        End If

        'Create hub.txt if it dosent exist.
        If Not File.Exists(DatabasePath & FileHub) Then
            'create hub.txt
            If Not CreateFile(DatabasePath & FileHub) Then
                Return False
            End If
            'TODO: [low priority] Message box tell the user they need to add some hub id's since we had to create it
        End If
        Return True
    End Function

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        lbMemory.Text = "Used memory: " & Format(GC.GetTotalMemory(True), "### ### ##0 bytes")
        lbLoadtime.Text = "Server started at: " & Now
        If Not InitFiles() Then
            Me.Close()
            Return
        End If
        StartServer()
        'Debug.Print(GetFlagCharectersFromFlag(AddFlagsToUser(0, "abcdefghijklmnopqrstuvwxyz")))
        'Debug.Print(GetFlagCharectersFromFlag(RemoveFlagsFromUser(AddFlagsToUser(0, "abcdefghijklmnopqrstuvwxyz"), "acegikmoqsuwy")))
    End Sub

    <System.MTAThreadAttribute()>
    Sub StartServer()
        timeBeginPeriod(1)  'Set timeGetTime to a accuracy of 1ms

        Dim dateTimeStarted As Date = Now
        Debug.Print("INFORMATION, [" & Format(TimeOfDay, "hh:mm:ss") & "] Server Starting...")

        ApplicationsPath = Application.StartupPath & "\"

        Dim currentDomain As AppDomain = AppDomain.CurrentDomain
        AddHandler currentDomain.UnhandledException, AddressOf GenericExceptionHandler
        AddHandler currentDomain.ProcessExit, AddressOf ApplicationTerminating

        WS = New WorldServerClass
        GC.Collect()

        Process.GetCurrentProcess().PriorityClass = ProcessPriorityClass.High
    End Sub

    Private Sub ApplicationTerminating(ByVal sender As Object, ByVal e As EventArgs)
        'neat trick here heh
        Return
    End Sub

    Private Sub GenericExceptionHandler(ByVal sender As Object, ByVal e As UnhandledExceptionEventArgs)
        Dim EX As Exception
        EX = e.ExceptionObject

        Debug.Print("CRITICAL, " & EX.ToString)
        Debug.Print("FAILED, Unexpected error has occured. An 'Error-yyyy-mmm-d-h-mm.log' file has been created.")

        Dim tw As TextWriter
        tw = New StreamWriter(New FileStream(String.Format("Error-{0}.log", Format(Now, "yyyy-MMM-d-H-mm")), FileMode.Create))
        tw.Write(EX.ToString)
        tw.Close()
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        lbMemory.Text = "Used memory: " & Format(GC.GetTotalMemory(True), "### ### ### ### ##0 bytes")
    End Sub

    Private Sub Form1_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        'Cleanup
        WS.Dispose()
    End Sub

End Class
