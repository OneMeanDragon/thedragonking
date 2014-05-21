﻿Imports System.Threading
Imports System.Reflection
Imports System.Text.RegularExpressions
Imports System.Net.Sockets
Imports System.Xml.Serialization
Imports System.IO
Imports System.Net
Imports System.Runtime.Remoting
Imports System.Runtime.CompilerServices
Imports System.Collections.Generic
Imports System.Security.Permissions

Public Module Network
    Public WS As WorldServerClass

    Class WorldServerClass
        Inherits MarshalByRefObject
        Implements IDisposable

        <CLSCompliant(False)> _
        Public m_flagStopListen As Boolean = False
        Private m_TimerPing As Timer
        Private m_TimerStats As Timer
        Private m_TimerCPU As Timer

        Private m_Socket As Socket
        Public Sub New()
            Try
                m_Socket = New Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
                m_Socket.Bind(New IPEndPoint(IPAddress.Any, 21845))
                m_Socket.Listen(5)
                m_Socket.BeginAccept(AddressOf AcceptConnection, Nothing)
                Debug.Print("LogType.SUCCESS Listening on " & IPAddress.Any.ToString & " on port " & 21845)

            Catch e As Exception
                Console.WriteLine()
                Debug.Print("LogType.FAILED, Error in " & e.Message & ": " & e.Source & ".", e.Message, e.Source)
            End Try
        End Sub
        Protected Sub AcceptConnection(ByVal ar As IAsyncResult)
            If m_flagStopListen Then Return

            Dim m_Client As New ClientClass
            m_Client.Socket = m_Socket.EndAccept(ar)
            'Thread.Sleep(100)
            'MessageBox.Show((m_Client.Socket Is m_Socket).ToString())
            m_Client.Socket.NoDelay = True
            m_Client.Socket.SetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.NoDelay, 1)

            m_Socket.BeginAccept(AddressOf AcceptConnection, Nothing)

            ThreadPool.QueueUserWorkItem(New System.Threading.WaitCallback(AddressOf m_Client.OnConnect))
        End Sub



        Public Sub Dispose() Implements IDisposable.Dispose
            m_flagStopListen = True
            m_Socket.Close()
        End Sub
        <SecurityPermissionAttribute(SecurityAction.Demand, Flags:=SecurityPermissionFlag.Infrastructure)> _
        Public Overrides Function InitializeLifetimeService() As Object
            Return Nothing
        End Function
    End Class

End Module
