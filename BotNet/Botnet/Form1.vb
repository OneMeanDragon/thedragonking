Imports System.Threading
Imports System.Net.Sockets
Imports System.Xml.Serialization
Imports System.IO
Imports System.Net
Imports System.Reflection
Imports System.Runtime.CompilerServices

Public Class Form1

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        lbMemory.Text = "Used memory: " & Format(GC.GetTotalMemory(True), "### ### ##0 bytes")
        lbLoadtime.Text = "Server started at: " & Now
        StartServer()
        'Debug.Print(GetFlagCharectersFromFlag(AddFlagsToUser(0, "abcdefghijklmnopqrstuvwxyz")))
        'Debug.Print(GetFlagCharectersFromFlag(RemoveFlagsFromUser(AddFlagsToUser(0, "abcdefghijklmnopqrstuvwxyz"), "acegikmoqsuwy")))
    End Sub

    <System.MTAThreadAttribute()> _
    Sub StartServer()
        timeBeginPeriod(1)  'Set timeGetTime to a accuracy of 1ms

        Dim dateTimeStarted As Date = Now
        Debug.Print("INFORMATION, [" & Format(TimeOfDay, "hh:mm:ss") & "] Server Starting...")

        ApplicationsPath = Application.StartupPath & "\"

        Dim currentDomain As AppDomain = AppDomain.CurrentDomain
        AddHandler currentDomain.UnhandledException, AddressOf GenericExceptionHandler

        WS = New WorldServerClass
        GC.Collect()

        Process.GetCurrentProcess().PriorityClass = ProcessPriorityClass.High
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
End Class
