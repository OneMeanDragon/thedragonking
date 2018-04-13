Imports System.Xml
Imports System.Reflection
Imports System.Threading
Imports System.Diagnostics.PerformanceCounter

'ABCDFILMRS

Public Module Functions
    Private ConnectionsHandled As Integer = 0
    Private ConnectionsPeak As Integer = 0
    Private ConnectionsCurrent As Integer = 0

    Public DataTransferOut As Long = 0
    Public DataTransferIn As Long = 0



    Public Sub ConnectionsIncrement()
        Interlocked.Increment(ConnectionsHandled)
        If Interlocked.Increment(ConnectionsCurrent) > ConnectionsPeak Then
            ConnectionsPeak = ConnectionsCurrent
        End If
    End Sub
    Public Sub ConnectionsDecrement()
        Interlocked.Decrement(ConnectionsCurrent)
    End Sub


    Public Function timeGetTime() As Integer
        Return System.Environment.TickCount()
    End Function
    Public Function timeBeginPeriod(ByVal uPeriod As Integer) As Integer
        Return 0
    End Function

    Public Function EscapeString(ByVal s As String) As String
        Return s.Replace("""", "").Replace("'", "")
    End Function

    Public Sub DumpPacket(ByVal data() As Byte, Optional ByRef Client As ClientClass = Nothing)
        '#If DEBUG Then
        Dim j As Integer
        Dim buffer As String = ""
        If Client Is Nothing Then
            buffer = buffer + [String].Format("DEBUG: Packet Dump{0}", vbNewLine)
        Else
            buffer = buffer + [String].Format("[{0}:{1}] DEBUG: Packet Dump{2}", Client.SocketData.IP, Client.SocketData.Port, vbNewLine)
        End If




        If data.Length Mod 16 = 0 Then
            For j = 0 To data.Length - 1 Step 16
                buffer += "|  " & BitConverter.ToString(data, j, 16).Replace("-", " ")
                buffer += " |  " & System.Text.Encoding.ASCII.GetString(data, j, 16).Replace(vbTab, "?").Replace(vbBack, "?").Replace(vbCr, "?").Replace(vbFormFeed, "?").Replace(vbLf, "?") & " |" & vbNewLine
            Next
        Else
            For j = 0 To data.Length - 1 - 16 Step 16
                buffer += "|  " & BitConverter.ToString(data, j, 16).Replace("-", " ")
                buffer += " |  " & System.Text.Encoding.ASCII.GetString(data, j, 16).Replace(vbTab, "?").Replace(vbBack, "?").Replace(vbCr, "?").Replace(vbFormFeed, "?").Replace(vbLf, "?") & " |" & vbNewLine
            Next

            buffer += "|  " & BitConverter.ToString(data, j, data.Length Mod 16).Replace("-", " ")
            buffer += New String(" ", (16 - data.Length Mod 16) * 3)
            buffer += " |  " & System.Text.Encoding.ASCII.GetString(data, j, data.Length Mod 16).Replace(vbTab, "?").Replace(vbBack, "?").Replace(vbCr, "?").Replace(vbFormFeed, "?").Replace(vbLf, "?")
            buffer += New String(" ", 16 - data.Length Mod 16)
            buffer += " |" & vbNewLine
        End If

        Debug.Print(buffer & vbNewLine)
        '#End If
    End Sub

    Public Enum FLAGS
        A = &H1
        B = &H2
        C = &H4
        D = &H8
        E = &H10
        F = &H20
        G = &H40
        H = &H80
        I = &H100
        J = &H200
        K = &H400
        L = &H800
        M = &H1000
        N = &H2000
        O = &H4000
        P = &H8000
        Q = &H10000
        R = &H20000
        S = &H40000
        T = &H80000
        U = &H100000
        V = &H200000
        W = &H400000
        X = &H800000
        Y = &H1000000
        Z = &H2000000
    End Enum
    Public Function GetFlagCharectersFromFlag(ByVal inflags As UInt32) As String
        Dim tmpOut As String = ""
        If (inflags And FLAGS.A) = FLAGS.A Then tmpOut += "A"
        If (inflags And FLAGS.B) = FLAGS.B Then tmpOut += "B"
        If (inflags And FLAGS.C) = FLAGS.C Then tmpOut += "C"
        If (inflags And FLAGS.D) = FLAGS.D Then tmpOut += "D"
        If (inflags And FLAGS.E) = FLAGS.E Then tmpOut += "E"
        If (inflags And FLAGS.F) = FLAGS.F Then tmpOut += "F"
        If (inflags And FLAGS.G) = FLAGS.G Then tmpOut += "G"
        If (inflags And FLAGS.H) = FLAGS.H Then tmpOut += "H"
        If (inflags And FLAGS.I) = FLAGS.I Then tmpOut += "I"
        If (inflags And FLAGS.J) = FLAGS.J Then tmpOut += "J"
        If (inflags And FLAGS.K) = FLAGS.K Then tmpOut += "K"
        If (inflags And FLAGS.L) = FLAGS.L Then tmpOut += "L"
        If (inflags And FLAGS.M) = FLAGS.M Then tmpOut += "M"
        If (inflags And FLAGS.N) = FLAGS.N Then tmpOut += "N"
        If (inflags And FLAGS.O) = FLAGS.O Then tmpOut += "O"
        If (inflags And FLAGS.P) = FLAGS.P Then tmpOut += "P"
        If (inflags And FLAGS.Q) = FLAGS.Q Then tmpOut += "Q"
        If (inflags And FLAGS.R) = FLAGS.R Then tmpOut += "R"
        If (inflags And FLAGS.S) = FLAGS.S Then tmpOut += "S"
        If (inflags And FLAGS.T) = FLAGS.T Then tmpOut += "T"
        If (inflags And FLAGS.U) = FLAGS.U Then tmpOut += "U"
        If (inflags And FLAGS.V) = FLAGS.V Then tmpOut += "V"
        If (inflags And FLAGS.W) = FLAGS.W Then tmpOut += "W"
        If (inflags And FLAGS.X) = FLAGS.X Then tmpOut += "X"
        If (inflags And FLAGS.Y) = FLAGS.Y Then tmpOut += "Y"
        If (inflags And FLAGS.Z) = FLAGS.Z Then tmpOut += "Z"
        'if flag = "" then need to figure out a default flag for a user
        Return tmpOut
    End Function
    Public Function GetFlagFromString(ByVal inflags As String) As UInt32
        Dim CurrentFlag As UInt32 = 0
        Dim i As UInt32 = 0
        For i = 0 To (inflags.Length - 1)
            Select Case (inflags.ToUpper)(i)
                Case "A"
                    If (CurrentFlag And FLAGS.A) = FLAGS.A Then Exit Select
                    CurrentFlag += FLAGS.A
                Case "B"
                    If (CurrentFlag And FLAGS.B) = FLAGS.B Then Exit Select
                    CurrentFlag += FLAGS.B
                Case "C"
                    If (CurrentFlag And FLAGS.C) = FLAGS.C Then Exit Select
                    CurrentFlag += FLAGS.C
                Case "D"
                    If (CurrentFlag And FLAGS.D) = FLAGS.D Then Exit Select
                    CurrentFlag += FLAGS.D
                Case "E"
                    If (CurrentFlag And FLAGS.E) = FLAGS.E Then Exit Select
                    CurrentFlag += FLAGS.E
                Case "F"
                    If (CurrentFlag And FLAGS.F) = FLAGS.F Then Exit Select
                    CurrentFlag += FLAGS.F
                Case "G"
                    If (CurrentFlag And FLAGS.G) = FLAGS.G Then Exit Select
                    CurrentFlag += FLAGS.G
                Case "H"
                    If (CurrentFlag And FLAGS.H) = FLAGS.H Then Exit Select
                    CurrentFlag += FLAGS.H
                Case "I"
                    If (CurrentFlag And FLAGS.I) = FLAGS.I Then Exit Select
                    CurrentFlag += FLAGS.I
                Case "J"
                    If (CurrentFlag And FLAGS.J) = FLAGS.J Then Exit Select
                    CurrentFlag += FLAGS.J
                Case "K"
                    If (CurrentFlag And FLAGS.K) = FLAGS.K Then Exit Select
                    CurrentFlag += FLAGS.K
                Case "L"
                    If (CurrentFlag And FLAGS.L) = FLAGS.L Then Exit Select
                    CurrentFlag += FLAGS.L
                Case "M"
                    If (CurrentFlag And FLAGS.M) = FLAGS.M Then Exit Select
                    CurrentFlag += FLAGS.M
                Case "N"
                    If (CurrentFlag And FLAGS.N) = FLAGS.N Then Exit Select
                    CurrentFlag += FLAGS.N
                Case "O"
                    If (CurrentFlag And FLAGS.O) = FLAGS.O Then Exit Select
                    CurrentFlag += FLAGS.O
                Case "P"
                    If (CurrentFlag And FLAGS.P) = FLAGS.P Then Exit Select
                    CurrentFlag += FLAGS.P
                Case "Q"
                    If (CurrentFlag And FLAGS.Q) = FLAGS.Q Then Exit Select
                    CurrentFlag += FLAGS.Q
                Case "R"
                    If (CurrentFlag And FLAGS.R) = FLAGS.R Then Exit Select
                    CurrentFlag += FLAGS.R
                Case "S"
                    If (CurrentFlag And FLAGS.S) = FLAGS.S Then Exit Select
                    CurrentFlag += FLAGS.S
                Case "T"
                    If (CurrentFlag And FLAGS.T) = FLAGS.T Then Exit Select
                    CurrentFlag += FLAGS.T
                Case "U"
                    If (CurrentFlag And FLAGS.U) = FLAGS.U Then Exit Select
                    CurrentFlag += FLAGS.U
                Case "V"
                    If (CurrentFlag And FLAGS.V) = FLAGS.V Then Exit Select
                    CurrentFlag += FLAGS.V
                Case "W"
                    If (CurrentFlag And FLAGS.W) = FLAGS.W Then Exit Select
                    CurrentFlag += FLAGS.W
                Case "X"
                    If (CurrentFlag And FLAGS.X) = FLAGS.X Then Exit Select
                    CurrentFlag += FLAGS.X
                Case "Y"
                    If (CurrentFlag And FLAGS.Y) = FLAGS.Y Then Exit Select
                    CurrentFlag += FLAGS.Y
                Case "Z"
                    If (CurrentFlag And FLAGS.Z) = FLAGS.Z Then Exit Select
                    CurrentFlag += FLAGS.Z
            End Select
        Next
        Return CurrentFlag
    End Function
    Public Function AddFlagsToUser(ByVal CurrentFlag As UInt32, ByVal sstrFlagsToAdd As String) As UInt32
        Dim i As UInt32, flagsOut As UInt32 = 0
        For i = 0 To (sstrFlagsToAdd.Length - 1)
            Select Case (sstrFlagsToAdd.ToUpper)(i)
                Case "A"
                    If (CurrentFlag And FLAGS.A) = FLAGS.A Then Exit Select
                    flagsOut += FLAGS.A
                Case "B"
                    If (CurrentFlag And FLAGS.B) = FLAGS.B Then Exit Select
                    flagsOut += FLAGS.B
                Case "C"
                    If (CurrentFlag And FLAGS.C) = FLAGS.C Then Exit Select
                    flagsOut += FLAGS.C
                Case "D"
                    If (CurrentFlag And FLAGS.D) = FLAGS.D Then Exit Select
                    flagsOut += FLAGS.D
                Case "E"
                    If (CurrentFlag And FLAGS.E) = FLAGS.E Then Exit Select
                    flagsOut += FLAGS.E
                Case "F"
                    If (CurrentFlag And FLAGS.F) = FLAGS.F Then Exit Select
                    flagsOut += FLAGS.F
                Case "G"
                    If (CurrentFlag And FLAGS.G) = FLAGS.G Then Exit Select
                    flagsOut += FLAGS.G
                Case "H"
                    If (CurrentFlag And FLAGS.H) = FLAGS.H Then Exit Select
                    flagsOut += FLAGS.H
                Case "I"
                    If (CurrentFlag And FLAGS.I) = FLAGS.I Then Exit Select
                    flagsOut += FLAGS.I
                Case "J"
                    If (CurrentFlag And FLAGS.J) = FLAGS.J Then Exit Select
                    flagsOut += FLAGS.J
                Case "K"
                    If (CurrentFlag And FLAGS.K) = FLAGS.K Then Exit Select
                    flagsOut += FLAGS.K
                Case "L"
                    If (CurrentFlag And FLAGS.L) = FLAGS.L Then Exit Select
                    flagsOut += FLAGS.L
                Case "M"
                    If (CurrentFlag And FLAGS.M) = FLAGS.M Then Exit Select
                    flagsOut += FLAGS.M
                Case "N"
                    If (CurrentFlag And FLAGS.N) = FLAGS.N Then Exit Select
                    flagsOut += FLAGS.N
                Case "O"
                    If (CurrentFlag And FLAGS.O) = FLAGS.O Then Exit Select
                    flagsOut += FLAGS.O
                Case "P"
                    If (CurrentFlag And FLAGS.P) = FLAGS.P Then Exit Select
                    flagsOut += FLAGS.P
                Case "Q"
                    If (CurrentFlag And FLAGS.Q) = FLAGS.Q Then Exit Select
                    flagsOut += FLAGS.Q
                Case "R"
                    If (CurrentFlag And FLAGS.R) = FLAGS.R Then Exit Select
                    flagsOut += FLAGS.R
                Case "S"
                    If (CurrentFlag And FLAGS.S) = FLAGS.S Then Exit Select
                    flagsOut += FLAGS.S
                Case "T"
                    If (CurrentFlag And FLAGS.T) = FLAGS.T Then Exit Select
                    flagsOut += FLAGS.T
                Case "U"
                    If (CurrentFlag And FLAGS.U) = FLAGS.U Then Exit Select
                    flagsOut += FLAGS.U
                Case "V"
                    If (CurrentFlag And FLAGS.V) = FLAGS.V Then Exit Select
                    flagsOut += FLAGS.V
                Case "W"
                    If (CurrentFlag And FLAGS.W) = FLAGS.W Then Exit Select
                    flagsOut += FLAGS.W
                Case "X"
                    If (CurrentFlag And FLAGS.X) = FLAGS.X Then Exit Select
                    flagsOut += FLAGS.X
                Case "Y"
                    If (CurrentFlag And FLAGS.Y) = FLAGS.Y Then Exit Select
                    flagsOut += FLAGS.Y
                Case "Z"
                    If (CurrentFlag And FLAGS.Z) = FLAGS.Z Then Exit Select
                    flagsOut += FLAGS.Z
            End Select
        Next
        Return (CurrentFlag + flagsOut)
    End Function
    Public Function RemoveFlagsFromUser(ByVal CurrentFlag As UInt32, ByVal sstrFlagsToAdd As String) As UInt32
        Dim i As UInt32, flagsOut As UInt32
        For i = 0 To (sstrFlagsToAdd.Length - 1)
            Select Case (sstrFlagsToAdd.ToUpper)(i)
                Case "A"
                    If (CurrentFlag And FLAGS.A) = FLAGS.A Then flagsOut -= FLAGS.A
                    Exit Select
                Case "B"
                    If (CurrentFlag And FLAGS.B) = FLAGS.B Then flagsOut -= FLAGS.B
                    Exit Select
                Case "C"
                    If (CurrentFlag And FLAGS.C) = FLAGS.C Then flagsOut -= FLAGS.C
                    Exit Select
                Case "D"
                    If (CurrentFlag And FLAGS.D) = FLAGS.D Then flagsOut -= FLAGS.D
                    Exit Select
                Case "E"
                    If (CurrentFlag And FLAGS.E) = FLAGS.E Then flagsOut -= FLAGS.E
                    Exit Select
                Case "F"
                    If (CurrentFlag And FLAGS.F) = FLAGS.F Then flagsOut -= FLAGS.F
                    Exit Select
                Case "G"
                    If (CurrentFlag And FLAGS.G) = FLAGS.G Then flagsOut -= FLAGS.G
                    Exit Select
                Case "H"
                    If (CurrentFlag And FLAGS.H) = FLAGS.H Then flagsOut -= FLAGS.H
                    Exit Select
                Case "I"
                    If (CurrentFlag And FLAGS.I) = FLAGS.I Then flagsOut -= FLAGS.I
                    Exit Select
                Case "J"
                    If (CurrentFlag And FLAGS.J) = FLAGS.J Then flagsOut -= FLAGS.J
                    Exit Select
                Case "K"
                    If (CurrentFlag And FLAGS.K) = FLAGS.K Then flagsOut -= FLAGS.K
                    Exit Select
                Case "L"
                    If (CurrentFlag And FLAGS.L) = FLAGS.L Then flagsOut -= FLAGS.L
                    Exit Select
                Case "M"
                    If (CurrentFlag And FLAGS.M) = FLAGS.M Then flagsOut -= FLAGS.M
                    Exit Select
                Case "N"
                    If (CurrentFlag And FLAGS.N) = FLAGS.N Then flagsOut -= FLAGS.N
                    Exit Select
                Case "O"
                    If (CurrentFlag And FLAGS.O) = FLAGS.O Then flagsOut -= FLAGS.O
                    Exit Select
                Case "P"
                    If (CurrentFlag And FLAGS.P) = FLAGS.P Then flagsOut -= FLAGS.P
                    Exit Select
                Case "Q"
                    If (CurrentFlag And FLAGS.Q) = FLAGS.Q Then flagsOut -= FLAGS.Q
                    Exit Select
                Case "R"
                    If (CurrentFlag And FLAGS.R) = FLAGS.R Then flagsOut -= FLAGS.R
                    Exit Select
                Case "S"
                    If (CurrentFlag And FLAGS.S) = FLAGS.S Then flagsOut -= FLAGS.S
                    Exit Select
                Case "T"
                    If (CurrentFlag And FLAGS.T) = FLAGS.T Then flagsOut -= FLAGS.T
                    Exit Select
                Case "U"
                    If (CurrentFlag And FLAGS.U) = FLAGS.U Then flagsOut -= FLAGS.U
                    Exit Select
                Case "V"
                    If (CurrentFlag And FLAGS.V) = FLAGS.V Then flagsOut -= FLAGS.V
                    Exit Select
                Case "W"
                    If (CurrentFlag And FLAGS.W) = FLAGS.W Then flagsOut -= FLAGS.W
                    Exit Select
                Case "X"
                    If (CurrentFlag And FLAGS.X) = FLAGS.X Then flagsOut -= FLAGS.X
                    Exit Select
                Case "Y"
                    If (CurrentFlag And FLAGS.Y) = FLAGS.Y Then flagsOut -= FLAGS.Y
                    Exit Select
                Case "Z"
                    If (CurrentFlag And FLAGS.Z) = FLAGS.Z Then flagsOut -= FLAGS.Z
                    Exit Select
            End Select
        Next
        Return (CurrentFlag - flagsOut)
    End Function
End Module
