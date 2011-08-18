Imports System
Imports System.Threading
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

Public Module ClientData
    Public CLIENTs As New Dictionary(Of UInteger, ClientClass)
    Public CLIETNIDs As Long = 0

    Public MAXLENGTH_HUB_ID As Integer = 32
    Public MAXLENGTH_HUB_PASSWORD As Integer = 64
    Public MAXLENGTH_BATTLENET_NAME As Integer = 20
    Public MAXLENGTH_BATTLENET_CHANNEL As Integer = 36
    Public MAXLENGTH_DATABASE_ID As Integer = 96
    Public MAXLENGTH_COMMAND As Integer = 384
    Public MAXLENGTH_SENDER As Integer = 32

    <Serializable()> _
    Public Class ClientInfo
        Public Index As UInteger
        Public IP As Net.IPAddress
        Public Port As UInteger
        Public Account As String = ""
        Public HUB_ID As String = ""
        Public HUB_PASSWORD As String = ""
        Public HUB_AUTHORIZED As Boolean = False
        Public HUB_FLAG As Int32 = 0
        'Public Account As String = ""
        Public Password As String = ""
        Public BattleNetName As String = ""
        Public BattleNetChannel As String = ""
        Public BattleNetIP As Long = 0
        Public IsCycleing As Long = 0
        Public DatabaseAccountID As String = ""
        Public DatabaseAccountPass As String = ""
        Public AccountUniqueID As Long = 0
        Public ChatDrop As Integer
        Public Authorized As Boolean = False
        Public LoggedIn As Boolean = False
        Public Flags As Long = 0
        Public COMMUNICATION_VERSION As Long = 0
        Public CLIENT_CAPABILITIES As Long = 0
    End Class

    Class ClientClass
        Inherits ClientInfo
        Implements IDisposable


        Public Socket As Socket = Nothing
        Public Queue As New Queue
        Private WithEvents KeepAliveTimer As Timers.Timer

        Protected SocketBuffer(8192) As Byte
        Protected SocketBytes As Integer

        Public DEBUG_CONNECTION As Boolean = False
        Private Buffer() As Byte = {0}

        Public Function GetClientInfo() As ClientInfo
            Dim ci As New ClientInfo

            ci.Account = Account
            ci.Index = Index
            ci.IP = IP
            ci.Port = Port

            Return ci
        End Function
        Private Function GetUniqueID() As Long
            Dim IsUnique As Boolean = False
            Dim TmpID As Long = 1
            Dim IdMatch As Boolean = False
            While IsUnique = False
                For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                    If cliTemp.Value.AccountUniqueID = TmpID Then
                        IdMatch = True
                        Exit For
                    End If
                Next
                If IdMatch Then
                    TmpID += 1
                    IdMatch = False
                Else
                    Return TmpID
                    Exit While
                End If
            End While
            Return -1
        End Function


        Public Sub OnConnect(ByVal state As Object)
            IP = CType(Socket.RemoteEndPoint, IPEndPoint).Address
            Port = CType(Socket.RemoteEndPoint, IPEndPoint).Port
            AccountUniqueID = GetUniqueID()
            Debug.Print("Incomming connection from [" & IP.ToString & ":" & Port & "]")

            Socket.BeginReceive(SocketBuffer, 0, SocketBuffer.Length, SocketFlags.None, AddressOf OnData, Nothing)

            Me.Index = Interlocked.Increment(CLIETNIDs)

            SyncLock CType(CLIENTs, ICollection).SyncRoot
                CLIENTs.Add(Me.Index, Me)
            End SyncLock

            ConnectionsIncrement()
        End Sub
        Public Sub OnData(ByVal ar As IAsyncResult)
            If Socket Is Nothing Then Return
            If Not Socket.Connected Then Return
            If WS.m_flagStopListen Then Return

            Try
                SocketBytes = Socket.EndReceive(ar)
                If SocketBytes = 0 Then
                    Me.Dispose()
                Else
                    Interlocked.Add(DataTransferIn, SocketBytes)

                    While SocketBytes > 0
                        'If Encryption Then Decode(SocketBuffer)

                        'Calculate Length from packet
                        Dim PacketLen As Integer = (SocketBuffer(2) + SocketBuffer(3) * 256) '+ 2

                        If SocketBytes < PacketLen Then
                            Debug.Print("CRITICAL, [" & IP.ToString & ":" & Port & "] BAD PACKET " & SocketBytes & "(" & PacketLen & ") bytes. ")
                        End If

                        'Move packet to Data
                        Dim data(PacketLen - 1) As Byte
                        Array.Copy(SocketBuffer, data, PacketLen)

                        'Create packet and add it to queue
                        Dim p As New PacketClass(data)
                        If p.OpCode <> OPCODES.PACKET_IDLE Then
                            KeepAliveTimer.Enabled = False
                            KeepAliveTimer.Interval = 120000
                            KeepAliveTimer.Enabled = True
                        End If
                        SyncLock Queue.SyncRoot
                            Queue.Enqueue(p)
                        End SyncLock

                        'Delete packet from buffer
                        SocketBytes -= PacketLen
                        Array.Copy(SocketBuffer, PacketLen, SocketBuffer, 0, SocketBytes)

                    End While

                    Socket.BeginReceive(SocketBuffer, 0, SocketBuffer.Length, SocketFlags.None, AddressOf OnData, Nothing)

                    ThreadPool.QueueUserWorkItem(AddressOf OnPacket)
                End If
            Catch Err As Exception
#If DEBUG Then
                'NOTE: If it's a error here it means the connection is closed?
                Debug.Print("WARNING, [" & IP.ToString & ":" & Port & "] cause error " & Err.ToString)
#End If
                Me.Dispose()
            End Try
        End Sub
        <MethodImplAttribute(MethodImplOptions.Synchronized)> _
        Public Sub OnPacket()
            While Queue.Count > 0
                Dim p As PacketClass

                SyncLock Queue.SyncRoot
                    p = Queue.Dequeue
                End SyncLock

                If PacketHandlers.ContainsKey(p.OpCode) = True Then
                    Try
                        PacketHandlers(p.OpCode).Invoke(p, Me)
                    Catch e As Exception
                        Debug.Print("FAILED, Opcode handler " & p.OpCode & " caused an error:" & e.Message)
                    End Try
                Else
                    'kill their connection.
                End If

                p.Dispose()
            End While
        End Sub

        Public Sub Send(ByVal data() As Byte)
            If Not Socket.Connected Then Exit Sub

            Try
                Socket.BeginSend(data, 0, data.Length, SocketFlags.None, AddressOf OnSendComplete, Nothing)
            Catch Err As Exception
                Debug.WriteLine("CRITICAL, Connection from [" & IP.ToString & ":" & Port & "] cause error " & Err.ToString)
                Delete()
            End Try
        End Sub
        Public Sub Send(ByRef packet As PacketClass, ByVal clSocket As Socket)
            If packet Is Nothing Then Throw New ApplicationException("Packet doesn't contain data!")

            If Not Socket.Connected Then Exit Sub

            Try
                Dim data As Byte() = packet.Data
                clSocket.BeginSend(data, 0, data.Length, SocketFlags.None, AddressOf OnSendComplete, Nothing)
            Catch Err As Exception
                Debug.WriteLine("CRITICAL, Connection from [" & IP.ToString & ":" & Port & "] cause error " & Err.ToString)
                Delete()
            End Try
            packet.Dispose()
        End Sub
        Public Sub Send(ByRef packet As PacketClass)
            If packet Is Nothing Then Throw New ApplicationException("Packet doesn't contain data!")
            If Socket Is Nothing Then
                Debug.Print(Me.AccountUniqueID & ":" & "bad socket?")
            End If
            If Not Socket.Connected Then Exit Sub

            Try
                Socket.BeginSend(packet.Data, 0, packet.Data.Length, SocketFlags.None, AddressOf OnSendComplete, Nothing)
                'Dim i As Integer = Socket.Send(packet.Data, 0, packet.Data.Length, SocketFlags.None)
            Catch SE As SocketException
                Debug.WriteLine("CRITICAL, Connection from [" & IP.ToString & ":" & Port & "] cause error " & SE.ToString)
                Delete()
            Catch Err As Exception
                Debug.WriteLine("CRITICAL, Connection from [" & IP.ToString & ":" & Port & "] cause error " & Err.ToString)
                Delete()
            End Try
            packet.Dispose()
        End Sub
        Public Sub SendMultiplyPackets(ByRef packet As PacketClass)
            If packet Is Nothing Then Throw New ApplicationException("Packet doesn't contain data!")

            If Not Socket.Connected Then Exit Sub

            Try
                Dim data As Byte() = packet.Data.Clone
                Socket.BeginSend(data, 0, data.Length, SocketFlags.None, AddressOf OnSendComplete, Nothing)
            Catch Err As Exception
                Debug.WriteLine("CRITICAL, Connection from [" & IP.ToString & ":" & Port & "] cause error " & Err.ToString)
                Delete()
            End Try
            'Don't forget to clean after using this function
        End Sub

        Public Sub OnSendComplete(ByVal ar As IAsyncResult)
            If Not Socket Is Nothing Then
                Try
                    Dim bytesSent As Integer = Socket.EndSend(ar)
                    Debug.Print("bytes sent: " & bytesSent)
                    Interlocked.Add(DataTransferOut, bytesSent)
                Catch SE As SocketException
                    Debug.WriteLine("CRITICAL, Connection from [" & IP.ToString & ":" & Port & "] cause error " & SE.ToString)
                    Delete()
                Catch Err As Exception
                    Debug.WriteLine("CRITICAL, Connection from [" & IP.ToString & ":" & Port & "] cause error " & Err.ToString)
                    Delete()
                End Try
            End If
        End Sub

        Private Sub Dispose() Implements System.IDisposable.Dispose
            If LoggedIn Then
                For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                    If cliTemp.Value.AccountUniqueID = AccountUniqueID Then
                        'do nothing do not send to the user logging off
                    Else
                        'notify everyone else user logged off
                        If cliTemp.Value.LoggedIn Then 'only if they are loggedin
                            Dim leave As New PacketClass(sOPCODES.PACKET_USERLOGGINGOFF)
                            leave.AddInt32(AccountUniqueID)
                            leave.AddInt32(&H7) '0x7??
                            cliTemp.Value.Send(leave)
                        End If
                    End If
                Next
            End If
            Debug.Print("NETWORK, Connection from [" & IP.ToString & ":" & Port & "] disposed ")

            On Error Resume Next

            If Not Socket Is Nothing Then Socket.Close()
            Socket = Nothing

            If Not KeepAliveTimer Is Nothing Then KeepAliveTimer.Enabled = False
            KeepAliveTimer = Nothing

            SyncLock CType(CLIENTs, ICollection).SyncRoot
                CLIENTs.Remove(Me.Index)
            End SyncLock

            ConnectionsDecrement()
        End Sub
        Public Sub Delete()
            On Error Resume Next
            Me.KeepAliveTimer.Enabled = False
            'Me.Socket = Nothing
            Me.Dispose()
        End Sub

        Public Sub IntializePacketHandlers()
            'PACKET_IDLE = 0             '(0x00)
            'PACKET_LOGON = 1            '(0x01)
            'PACKET_STATSUPDATE = 2      '(0x02)
            'PACKET_DATABASE = 3         '(0x03)
            'PACKET_MESSAGE = 4          '(0x04)
            'PACKET_CYCLE = 5            '(0x05)
            'PACKET_USERINFO = 6         '(0x06)
            'PACKET_BROADCASTMESSAGE = 7 '(0x07)
            'PACKET_USERLOGGINGOFF = 7   '(0x07)
            'PACKET_COMMAND = 8          '(0x08)
            'PACKET_CHANGEDBPASSWORD = 9 '(0x09)
            'PACKET_BOTNETVERSION = 10   '(0x0a)
            'PACKET_BOTNETCHAT = 11      '(0x0b)
            'PACKET_ACCOUNT = 12         '(0x0d)
            'PACKET_CHATDROPOPTIONS = 13 '(0x10)

            PacketHandlers(OPCODES.PACKET_IDLE) = CType(AddressOf OnUnhandledPacket, HandlePacket)
            PacketHandlers(OPCODES.PACKET_LOGON) = CType(AddressOf CMSG_PACKET_LOGON, HandlePacket)
            PacketHandlers(OPCODES.PACKET_STATSUPDATE) = CType(AddressOf CMSG_PACKET_STATSUPDATE, HandlePacket)
            PacketHandlers(OPCODES.PACKET_DATABASE) = CType(AddressOf OnUnhandledPacket, HandlePacket)
            PacketHandlers(OPCODES.PACKET_MESSAGE) = CType(AddressOf CMSG_PACKET_MESSAGE, HandlePacket)
            PacketHandlers(OPCODES.PACKET_CYCLE) = CType(AddressOf OnUnhandledPacket, HandlePacket)
            PacketHandlers(OPCODES.PACKET_USERINFO) = CType(AddressOf CMSG_PACKET_USERINFO, HandlePacket)
            PacketHandlers(OPCODES.PACKET_BROADCASTMESSAGE) = CType(AddressOf OnUnhandledPacket, HandlePacket)
            PacketHandlers(OPCODES.PACKET_COMMAND) = CType(AddressOf CMSG_PACKET_COMMAND, HandlePacket)
            PacketHandlers(OPCODES.PACKET_CHANGEDBPASSWORD) = CType(AddressOf OnUnhandledPacket, HandlePacket)
            PacketHandlers(OPCODES.PACKET_BOTNETVERSION) = CType(AddressOf CMSG_PACKET_BOTNETVERSION, HandlePacket)
            PacketHandlers(OPCODES.PACKET_BOTNETCHAT) = CType(AddressOf CMSG_PACKET_BOTNETCHAT, HandlePacket)
            PacketHandlers(OPCODES.PACKET_ACCOUNT) = CType(AddressOf CMSG_PACKET_ACCOUNT, HandlePacket)
            PacketHandlers(OPCODES.PACKET_CHATDROPOPTIONS) = CType(AddressOf OnUnhandledPacket, HandlePacket)
        End Sub
        Public Sub OnUnhandledPacket(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Debug.Print("LogType.WARNING, [" & Client.IP.ToString & ":" & Client.Port.ToString & "] " & CType(packet.OpCode, OPCODES) & " [Unhandled Packet]" & " [Protocol version: " & packet.ProtocalVersion & "]")
        End Sub
#Region "Response Enumerators"
        Private Enum HUB_ID_RESPONCES
            FAIL = 0
            ACCEPTED = 1
        End Enum
        Private Enum PACKET_ACCOUNT_COMMANDS
            LOGIN = 0
            CHANGE_PASSWORD = 1
            CREATE_ACCOUNT = 2
        End Enum
        Private Enum PACKET_ACCOUNT_RESULTS
            PASSED = 1
            FAILED = 0
        End Enum
        Private Enum PACKET_COMMAND_COMMANDS
            BROADCAST_TO_ALL_USERS = 0
            SEND_TO_DATABASE = 1
            DIRECTED_TO_SPECIFIC_CLIENT = 2
        End Enum
#Region "VIOLATION ERROR CODES"
        Private Enum PROTOCOL_VIOLATION_COMMAND_1
            CLIENT_ATTEMPTED_AUTHING_A_SECOND_TIME = 2
            BAD_STRLEN_HUBID = 3
            BAD_STRLEN_HUB_PASSWORD = 4
            STR_BLANK_HUBID = 5
            STR_BLANK_HUB_PASSWORD = 6
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_2
            CLIENT_NOTYET_AUTHORIZED = 2
            LEN_PACKET_TO_SMALL = 3
            LEN_VIOLATES_BATTLENET_NAME_LENGTH = 4
            LEN_VIOLATES_CHANNEL_NAME_LENGTH = 5
            MISSING_SERVER_IP = 6
            BAD_DATABASE_SIZE = 7
            MISSING_CYCLE_STATUS = 8
            BLANK_BATTLENET_NAME_EMPTY = 9
            BLANK_BATTLENET_CHANNELNAME_EMPTY = 10
            BAD_SERVER_IP = 11
            BADLYFORMED_BATTLENET_NAME = 12
            BADLYFORMED_DATABASE_STRING = 13
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_3
            MISSING_SUBCOMMAND = 2
            BAD_STATE = 3
            INVALID_SUBCOMMAND = 4
            SC2_MISSING_ENTRYNAME = 5
            SC2_MISSING_ENTRYFLAGS = 6
            SC2_EMPTY_NAME = 7
            SC2_EMPTY_FLAGS = 8
            SC2_MALFORMED_ENTRY = 9
            SC2_MALFORMED_FLAGS = 10
            SC3_PACKET_TO_SMALL = 11
            SC3_MISSING_ENTRYNAME = 12
            SC3_EMPTY_ENTRYNAME = 13
            SC3_MALFORMED_ENTRYNAME = 14
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_5
            BAD_STATE = 2
            MISSING_COUNT = 3
            COUNT_IS_ZERO = 4
            NOT_ENOUGH_DATA_IN_PACKET_TO_SATISFY_COUNT = 5
            SPECIFIED_MORE_CLIENTS_THAN_EXIST_ON_BOTNET = 6
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_6
            BAD_STATE = 2
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_7
            BAD_STATE = 2
            MISSING_SENDER_SRTING = 3
            MISSING_COMMAND = 4
            EMPTY_SENDER = 5
            EMPTY_COMMAND = 6
            MALFORMED_SENDERNAME = 7
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_8
            BAD_STATE = 2
            NO_TARGET_ID = 3
            NO_SENDER_STRING = 4
            NO_COMMAND_STRING = 5
            BAD_TARGET_ID = 6
            EMPTY_SENDER = 7
            EMPTY_COMMAND = 8
            MALFORMED_SENDER_NAME = 9
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_9
            BAD_STATE = 2
            MISSING_SELECTOR_DWORD = 3
            MISSING_NEW_PASSWORD = 4
            MALFORMED_PASSWORD = 5
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_10
            MISSING_FIRST_DWORD = 2
            MISSING_SECOND_DWORD = 3
            UNEXPECTED_FLAGS_IN_SECOND_DWORD = 4
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_11
            BAD_STATE = 2
            MISSING_DISTRIBUTION_LEVEL = 3
            MISSING_EMOTE = 4
            MISSING_TARGET_ID = 5
            MISSING_TEXT = 6
            BAD_DISTRIBUTION_LEVEL = 7
            BAD_TARGET = 8 '(only if attempting to whisper a user)
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_12
            BAD_STATE = 2
            MISSING_SUBCOMMAND = 3
            '	Other errors are subcommand specific.
        End Enum
        Private Enum PROTOCOL_VIOLATION_COMMAND_13
            MISSING_SUBCOMMAND = 1
            BAD_SUBCOMMAND = 2
            MISSING_USERNAME = 3
            EMPTY_USERNAME = 4
            MISSING_PASSWORD = 5
            EMPTY_PASSWORD = 6
            SC1_MISSING_NEWPASSWORD = 7
            SC1_EMPTY_NEWPASSWORD = 8
        End Enum
#End Region
#End Region
        '    (send to client) id 0x08: protocol violation
        'Contents:
        '	(DWORD) Error code.  This is message specific.
        '	(BYTE) ID of the offending command
        '	(WORD) Length of the offending packet
        '	(WORD) Length of unprocessed data (length of packets which arrived after the offending packets)
        'Response: None.  The server closes the botnet socket immediately after
        'sending this message.
        'Error codes:
        '	1 = unrecognized command ID.  Any other code is command specific

        Private Sub PROTOCOL_VIOLATION(ByRef Client As ClientClass, ByVal ErrorFrom As OPCODES, ByVal ErrorID As Integer, ByVal pLength As Long, ByVal pRemainingLen As Long)
            Dim response As New PacketClass(OPCODES.PACKET_PROTOCOL_VIOLATION)
            response.AddInt32(ErrorID)
            response.AddInt8(ErrorFrom)
            response.AddInt16(pLength)
            response.AddInt16(pRemainingLen)
            Client.Send(response)

            Call Client.Delete()
            '	(DWORD) Error code.  This is message specific.
            '	(BYTE) ID of the offending command
            '	(WORD) Length of the offending packet
            '	(WORD) Length of unprocessed data (length of packets which arrived after the offending packets)
        End Sub

        Public Sub CMSG_PACKET_BOTNETVERSION(ByRef packet As PacketClass, ByRef Client As ClientClass)
            '(send to server) id 0x0a: specify communication version and client
            '	capabilities
            'Contents:
            '	(DWORD) communication version.  Valid values are 0 (the default), or 1.
            '	Messages which have conditionally added fields (as identified by the
            '	4.X syntax) will contain fields for which X is not above this value.
            '	(DWORD) client capabilities.  Currently only bit 0 is defined.  If set,
            '	the client awaits server confirmation of database changes.  If clear,
            '	the client updates the local ACL immediately and expects the server to
            '	countermand prohibited changes.
            'Response: The server updates the communication version and sends 0x9 to the
            '	client.  Message 0xa may be sent at any time, and may be resent if the
            '	client desires to change the negotiation version.
            Client.COMMUNICATION_VERSION = packet.GetInt32
            Client.CLIENT_CAPABILITIES = packet.GetInt32
            '(send to client) id 0x09: acknowledge communication version
            'Contents:
            '	(DWORD) communication version.  This message is sent to confirm
            '	acceptance of msg 0x0a.  All subsequent messages will be formed in
            '	this style.  That is, clients should not change parsing methods until
            '	the server confirms the new style.
            'Response:   None()
            Dim response As New PacketClass(OPCODES.PACKET_CHANGEDBPASSWORD)
            response.AddInt32(1) '4.0 0->1 4.1
            Client.Send(response)
        End Sub

        Public Sub CMSG_PACKET_BOTNETCHAT(ByRef packet As PacketClass, ByRef Client As ClientClass)
            '	(DWORD) command
            '		0	: message to all bots
            '		->1	: message to bots on the same database
            '		2	: message to bot specified by id.
            '	(DWORD) action	: 0x00=talk, 0x01=emote, any other is dropped
            '	(DWORD) id	: for command 0x02, id of bot to send to, otherwise ignored.
            '	(STRING:496) message: blank messages are dropped
            Dim command As Long = packet.GetInt32
            If (command < 0) Or (command > 2) Then
                'bad command
                Return
            End If
            Dim action As Long = packet.GetInt32
            If (action < 0) Or (action > 1) Then
                'bad action
                Return
            End If
            Dim userid As Long = packet.GetInt32
            Dim message As String = packet.GetString
            If Trim(message) = "" Then
                'bad message
                Return
            End If
            Select Case command
                Case PACKET_COMMAND_COMMANDS.BROADCAST_TO_ALL_USERS
                    CHAT_MESSAGE_TO_ALL_USERS(Client, command, action, message)
                Case PACKET_COMMAND_COMMANDS.SEND_TO_DATABASE
                    CHAT_MESSAGE_ALL_ON_DATABASE(Client, command, action, message)
                Case PACKET_COMMAND_COMMANDS.DIRECTED_TO_SPECIFIC_CLIENT
                    CHAT_MESSAGE_TO_USER(Client, command, action, userid, message)
            End Select
        End Sub

        Private Sub CHAT_MESSAGE_TO_ALL_USERS(ByRef Client As ClientClass, ByVal command As Long, ByVal action As Long, ByVal message As String)
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If cliTemp.Value.LoggedIn Then
                    If Not (cliTemp.Value.Account = Client.Account) Then
                        Dim sendmessage As New PacketClass(OPCODES.PACKET_BOTNETCHAT)
                        sendmessage.AddInt32(command)
                        sendmessage.AddInt32(action)
                        sendmessage.AddInt32(Client.AccountUniqueID)
                        sendmessage.AddString(message)
                        cliTemp.Value.Send(sendmessage)
                    End If
                End If
            Next
        End Sub
        Private Sub CHAT_MESSAGE_TO_USER(ByRef Client As ClientClass, ByVal command As Long, ByVal action As Long, ByVal userid As Long, ByVal message As String)
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If cliTemp.Value.AccountUniqueID = userid Then
                    If cliTemp.Value.LoggedIn Then
                        Dim sendmessage As New PacketClass(OPCODES.PACKET_BOTNETCHAT)
                        sendmessage.AddInt32(command)
                        sendmessage.AddInt32(action)
                        sendmessage.AddInt32(Client.AccountUniqueID)
                        sendmessage.AddString(message)
                        cliTemp.Value.Send(sendmessage)
                    End If
                    Return 'exit the loop
                End If
            Next
        End Sub
        Private Sub CHAT_MESSAGE_ALL_ON_DATABASE(ByRef Client As ClientClass, ByVal command As Long, ByVal action As Long, ByVal message As String)
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If cliTemp.Value.DatabaseAccountID = Client.DatabaseAccountID Then
                    If Not (cliTemp.Value.Account = Client.Account) Then
                        If cliTemp.Value.LoggedIn Then
                            Dim sendmessage As New PacketClass(OPCODES.PACKET_BOTNETCHAT)
                            sendmessage.AddInt32(command)
                            sendmessage.AddInt32(action)
                            sendmessage.AddInt32(Client.AccountUniqueID)
                            sendmessage.AddString(message)
                            cliTemp.Value.Send(sendmessage)
                        End If
                    End If
                End If
            Next
        End Sub
        Public Sub CMSG_PACKET_MESSAGE(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Dim sender As String = packet.GetString
            Dim command As String = packet.GetString
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If cliTemp.Value.DatabaseAccountID = Client.DatabaseAccountID Then
                    If cliTemp.Value.LoggedIn Then
                        'do nothing do not send to the user logging in
                        '(send to client) id 0x04: command over botnet
                        'Contents:
                        '	(4.1) (DWORD) sending client's ID
                        '	(4.1) (DWORD) distribution status
                        '		0 = broadcast to all users
                        '		1 = sent to database
                        '		2 = directed to this client specifically
                        '	(STRING) sender
                        '	(STRING) command
                        'Response: None.
                        Dim sendcommand As New PacketClass(OPCODES.PACKET_MESSAGE)
                        sendcommand.AddInt32(Client.AccountUniqueID)                    '4.1
                        sendcommand.AddInt32(PACKET_COMMAND_COMMANDS.SEND_TO_DATABASE)  '4.1
                        sendcommand.AddString(Client.Account)
                        sendcommand.AddString(command)
                        cliTemp.Value.Send(sendcommand)
                    Else
                        'Cant send to this user they're not logged in
                    End If
                End If
            Next
        End Sub
        Public Sub CMSG_PACKET_COMMAND(ByRef packet As PacketClass, ByRef Client As ClientClass)
            If packet.Data.Length < 10 Then 'header+dword+null+null
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Dim target As Long = packet.GetInt32
            If target <= 0 Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_TARGET_ID, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Dim sender As String = packet.GetString
            If sender.Length > MAXLENGTH_SENDER Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.MALFORMED_SENDER_NAME, packet.Length, (packet.Length - packet.Offset))
                Return
            ElseIf sender = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.EMPTY_SENDER, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Dim command As String = packet.GetString
            If command.Length > MAXLENGTH_COMMAND Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                Return
            ElseIf command = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.EMPTY_COMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If Not Client.LoggedIn Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Dim tarFound As Boolean = False
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If cliTemp.Value.AccountUniqueID = target Then
                    tarFound = True
                    If cliTemp.Value.LoggedIn Then
                        'do nothing do not send to the user logging in
                        '(send to client) id 0x04: command over botnet
                        'Contents:
                        '	(4.1) (DWORD) sending client's ID
                        '	(4.1) (DWORD) distribution status
                        '		0 = broadcast to all users
                        '		1 = sent to database
                        '		2 = directed to this client specifically
                        '	(STRING) sender
                        '	(STRING) command
                        'Response: None.
                        Dim sendcommand As New PacketClass(OPCODES.PACKET_MESSAGE)
                        sendcommand.AddInt32(Client.AccountUniqueID)                                '4.1
                        sendcommand.AddInt32(PACKET_COMMAND_COMMANDS.DIRECTED_TO_SPECIFIC_CLIENT)   '4.1
                        sendcommand.AddString(Client.Account)
                        sendcommand.AddString(command)
                        cliTemp.Value.Send(sendcommand)
                    Else
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If
                End If
            Next
            If Not tarFound Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_TARGET_ID, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
        End Sub

        Public Sub CMSG_PACKET_USERINFO(ByRef packet As PacketClass, ByRef Client As ClientClass)
            If Client.Authorized Then
                If Client.LoggedIn = False Then
                    'Log them in send everyone else the update
                    For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                        If cliTemp.Value.LoggedIn = False Then
                            'do nothing do not send to the user logging in
                        Else
                            'notify everyone else user logged on
                            '	(DWORD) bot id
                            '	(4.1) (DWORD) database access flags
                            '		1 = read
                            '		2 = write
                            '		4 = restricted access
                            '	(4.1) (DWORD) administrative capabilities
                            '		Specified in Zerobot Traditional Flags Format (ZTFF):
                            '		A = superuser, can perform any administrative action
                            '		B = broadcast, may use talk-to-all
                            '		C = connection, may administer botnet connectivity
                            '		D = database, may create and maintain databases
                            '		I = ID control, may create and modify hub IDs
                            '		S = botnet service
                            '	(4.1) (Admin only) (DWORD) IP address of the bot being described
                            '           Sent only to users with flags (A or L)
                            '	(STRING:20) bot name
                            '	(STRING:*) bot channel
                            '	(DWORD) bot server
                            '	(2) (STRING:16) unique account name
                            '	(3) (STRING:*) database
                            Dim joined As New PacketClass(OPCODES.PACKET_USERINFO)
                            joined.AddInt32(Client.AccountUniqueID)
                            '4.1 here
                            joined.AddInt32(GetDatabaseFlag(Client.Account))
                            joined.AddInt32(GetAccountFlag(Client.Account))
                            'TODO:
                            'If ((cliTemp.Value.AccountFlag And FLAGS.A_) = FLAGS.A_) Or ((cliTemp.Value.AccountFlag And FLAGS.L_) = FLAGS.L_) Then
                            '   joined.AddByteArray(Client.IP.GetAddressBytes())
                            'End If
                            joined.AddString(Client.BattleNetName)
                            joined.AddString(Client.BattleNetChannel)
                            joined.AddInt32(Client.BattleNetIP)
                            joined.AddString(Client.Account)
                            joined.AddString(Client.DatabaseAccountID)
                            cliTemp.Value.Send(joined)
                        End If
                    Next
                    'log client in, send client user details of those that are on the server.
                    Client.LoggedIn = True
                    'send to self first
                    For Each imLoggedIn As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                        If imLoggedIn.Value.AccountUniqueID = Client.AccountUniqueID Then
                            Dim ImLoggedInResponse As New PacketClass(OPCODES.PACKET_USERINFO)
                            ImLoggedInResponse.AddInt32(imLoggedIn.Value.AccountUniqueID)
                            '4.1 here
                            ImLoggedInResponse.AddInt32(GetDatabaseFlag(imLoggedIn.Value.Account))
                            ImLoggedInResponse.AddInt32(GetAccountFlag(imLoggedIn.Value.Account))
                            'TODO:
                            'If ((imLoggedIn.Value.AccountFlag And FLAGS.A_) = FLAGS.A_) Or ((imLoggedIn.Value.AccountFlag And FLAGS.L_) = FLAGS.L_) Then
                            '   ImLoggedInResponse.AddByteArray(imLoggedIn.IP.GetAddressBytes())
                            'End If
                            ImLoggedInResponse.AddString(imLoggedIn.Value.BattleNetName)
                            ImLoggedInResponse.AddString(imLoggedIn.Value.BattleNetChannel)
                            ImLoggedInResponse.AddInt32(imLoggedIn.Value.BattleNetIP)
                            ImLoggedInResponse.AddString(imLoggedIn.Value.Account)
                            ImLoggedInResponse.AddString(imLoggedIn.Value.DatabaseAccountID)
                            Client.Send(ImLoggedInResponse)
                            Exit For
                        End If
                    Next
                    For Each cliTemp2 As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                        If Not (cliTemp2.Value.LoggedIn = False) Then
                            If Not cliTemp2.Value.AccountUniqueID = Client.AccountUniqueID Then
                                Dim response As New PacketClass(OPCODES.PACKET_USERINFO)
                                response.AddInt32(cliTemp2.Value.AccountUniqueID)
                                '4.1 here
                                response.AddInt32(GetDatabaseFlag(cliTemp2.Value.Account))
                                response.AddInt32(GetAccountFlag(cliTemp2.Value.Account))
                                'If ((cliTemp2.Value.AccountFlag And FLAGS.A_) = FLAGS.A_) Or ((cliTemp2.Value.AccountFlag And FLAGS.L_) = FLAGS.L_) Then
                                '   response.AddByteArray(cliTemp2.IP.GetAddressBytes())
                                'End If
                                response.AddString(cliTemp2.Value.BattleNetName)
                                response.AddString(cliTemp2.Value.BattleNetChannel)
                                response.AddInt32(cliTemp2.Value.BattleNetIP)
                                response.AddString(cliTemp2.Value.Account)
                                response.AddString(cliTemp2.Value.DatabaseAccountID)
                                Client.Send(response)
                            End If
                        End If
                    Next

                Else
                    '
                End If
            Else
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_USERINFO, PROTOCOL_VIOLATION_COMMAND_6.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Debug.Print("CMSG_PACKET_USERINFO" & vbNewLine)
        End Sub

        Private Function IsUserNameOnLine(ByVal AccountName As String) As Boolean
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If cliTemp.Value.Account.ToLower = AccountName.ToLower Then
                    Return True
                    Exit For
                End If
            Next
            Return False
        End Function
        Private Sub KickUser(ByVal AccountName As String)
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If cliTemp.Value.Account.ToLower = AccountName.ToLower Then
                    cliTemp.Value.Delete()
                    Return
                End If
            Next
        End Sub

        'TODO: setup an actuall database
        Public Sub CMSG_PACKET_ACCOUNT(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Dim command As Integer = packet.GetInt32
            If (command < 0) Or (command > 2) Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.BAD_SUBCOMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Dim accName As String = ""
            Dim accPass As String = ""
            Dim accOldPass As String = ""
            Dim accNewPass As String = ""
            Dim response As New PacketClass(OPCODES.PACKET_ACCOUNT)
            Select Case command
                Case PACKET_ACCOUNT_COMMANDS.LOGIN
                    'For Command 0x00 (Login):
                    '(STRING) 	 Account name
                    '(STRING) 	 Account password
                    accName = packet.GetString
                    If accName = "" Then
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.EMPTY_USERNAME, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If
                    Dim UserIsLoggedOn As Boolean = IsUserNameOnLine(accName)
                    accPass = packet.GetString
                    If accPass = "" Then
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.EMPTY_PASSWORD, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If
                    response.AddInt32(PACKET_ACCOUNT_COMMANDS.LOGIN)

                    response.AddInt32(PACKET_ACCOUNT_RESULTS.PASSED)
                    Client.Authorized = True 'If the users password is accepted then
                    Client.Account = accName
                    Client.Password = accPass
                    Client.Send(response)
                Case PACKET_ACCOUNT_COMMANDS.CHANGE_PASSWORD
                    'For Command 0x01 (Change password):
                    '(STRING) 	 Account name
                    '(STRING) 	 Old password
                    '(STRING) 	 New password
                    accName = packet.GetString
                    If accName = "" Then
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.EMPTY_USERNAME, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If
                    accOldPass = packet.GetString
                    If accOldPass = "" Then
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.EMPTY_PASSWORD, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If
                    accNewPass = packet.GetString
                    If accNewPass = "" Then
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.SC1_EMPTY_NEWPASSWORD, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If
                    response.AddInt32(PACKET_ACCOUNT_COMMANDS.CHANGE_PASSWORD)
                    response.AddInt32(PACKET_ACCOUNT_RESULTS.FAILED)
                    Client.Send(response)
                Case PACKET_ACCOUNT_COMMANDS.CREATE_ACCOUNT
                    'For Command 0x02 (Create account):
                    '(STRING) 	 Account name
                    '(STRING) 	 Account password 
                    accName = packet.GetString
                    If accName = "" Then
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.EMPTY_USERNAME, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If
                    accPass = packet.GetString
                    If accPass = "" Then
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.EMPTY_PASSWORD, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If
                    response.AddInt32(PACKET_ACCOUNT_COMMANDS.CREATE_ACCOUNT)
                    response.AddInt32(PACKET_ACCOUNT_RESULTS.FAILED)
                    'Client.Authorized = True 'If the users password is accepted then
                    Client.Account = accName
                    Client.Password = accPass
                    Client.Send(response)
                Case Else
                    '
            End Select
            Debug.Print("CMSG_PACKET_ACCOUNT" & vbNewLine)
        End Sub

        Public Sub CMSG_PACKET_STATSUPDATE(ByRef packet As PacketClass, ByRef Client As ClientClass)
            If Not Client.HUB_AUTHORIZED Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.CLIENT_NOTYET_AUTHORIZED, packet.Length, (packet.Length - packet.Offset))
                Return
            ElseIf Not Client.Authorized Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.CLIENT_NOTYET_AUTHORIZED, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Client.BattleNetName = packet.GetString
            If Client.BattleNetName.Length > MAXLENGTH_BATTLENET_NAME Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.LEN_VIOLATES_BATTLENET_NAME_LENGTH, packet.Length, packet.Offset)
                Return
            ElseIf Client.BattleNetName = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BLANK_BATTLENET_NAME_EMPTY, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Client.BattleNetChannel = packet.GetString
            If Client.BattleNetChannel.Length > MAXLENGTH_BATTLENET_CHANNEL Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.LEN_VIOLATES_CHANNEL_NAME_LENGTH, packet.Length, packet.Offset)
                Return
            ElseIf Client.BattleNetChannel = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BLANK_BATTLENET_CHANNELNAME_EMPTY, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Client.BattleNetIP = packet.GetInt32
            Dim tmpStr As String = packet.GetString
            If tmpStr.Length > MAXLENGTH_DATABASE_ID Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BAD_DATABASE_SIZE, packet.Length, packet.Offset)
                Return
            ElseIf tmpStr = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BADLYFORMED_DATABASE_STRING, packet.Length, (packet.Length - packet.Offset))
                Return
            ElseIf tmpStr.Split(" ").Length <> 2 Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BADLYFORMED_DATABASE_STRING, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Dim DatabaseData() As String = tmpStr.Split(" ")
            Client.DatabaseAccountID = DatabaseData(0)
            Client.DatabaseAccountPass = DatabaseData(1) 'System.Text.Encoding.UTF8.GetBytes(tmpStr.Split(" ")(1))

            Client.IsCycleing = packet.GetInt32
            If (Client.IsCycleing < 0) Or (Client.IsCycleing > 1) Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BAD_DATABASE_SIZE, packet.Length, packet.Offset)
                Return
            End If

            Dim response As New PacketClass(OPCODES.PACKET_STATSUPDATE)
            'to do: check database if account+pass match
            response.AddInt32(1) '1=sucess 0=fail 'currently allways success
            Client.Send(response)

            Debug.Print("CMSG_PACKET_STATSUPDATE" & vbNewLine)
        End Sub

        'Updated for Version #5 'Added violation sends for fuckups.
        Public Sub CMSG_PACKET_LOGON(ByRef packet As PacketClass, ByRef Client As ClientClass)
            If Client.HUB_AUTHORIZED Then 'Tryed to logon again after allready loging on
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.CLIENT_ATTEMPTED_AUTHING_A_SECOND_TIME, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If packet.ProtocalVersion < BOTNET_PROTO_VERSION Then
                Dim VerResponse As New PacketClass(OPCODES.PACKET_BOTNETVERSION)
                VerResponse.AddInt32(BOTNET_PROTO_VERSION)
                Client.Send(VerResponse)
            End If

            Client.HUB_ID = packet.GetString
            If Client.HUB_ID.Length > MAXLENGTH_HUB_ID Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.BAD_STRLEN_HUBID, packet.Length, packet.Offset)
                Return
            ElseIf Client.HUB_ID = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.STR_BLANK_HUBID, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Client.HUB_PASSWORD = packet.GetString
            If Client.HUB_PASSWORD.Length > MAXLENGTH_HUB_PASSWORD Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.BAD_STRLEN_HUB_PASSWORD, packet.Length, (packet.Length - packet.Offset))
                Return
            ElseIf Client.HUB_PASSWORD = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.STR_BLANK_HUB_PASSWORD, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim HUB_RESPONCE As HUB_ID_RESPONCES
            If ValidateHubAccount(Client.HUB_ID, Client.HUB_PASSWORD) Then
                HUB_RESPONCE = HUB_ID_RESPONCES.ACCEPTED
            Else
                HUB_RESPONCE = HUB_ID_RESPONCES.FAIL
                Me.Delete()
                Return
            End If

            Dim response As New PacketClass(OPCODES.PACKET_LOGON)
            response.AddInt32(HUB_RESPONCE)
            Client.HUB_AUTHORIZED = True 'If the users password is accepted then
            'If packet.ProtocalVersion >= 5 Then                     'Version 5
            '    response.AddInt8(Client.IP.GetAddressBytes.Length)  'IPv4|IPv6 Packet Update
            'End If                                                  'this will only send to users version 5+
            response.AddByteArray(Client.IP.GetAddressBytes)
            Client.Send(response)
            Debug.Print("CMSG_PACKET_LOGON" & vbNewLine)
        End Sub

        Public Sub New()
            IntializePacketHandlers()
            KeepAliveTimer = New Timers.Timer
            KeepAliveTimer.Interval = 180000
            KeepAliveTimer.Enabled = False
        End Sub

        Private Sub KeepAliveTimer_Elapsed(ByVal sender As Object, ByVal e As System.Timers.ElapsedEventArgs) Handles KeepAliveTimer.Elapsed
            Dim klPacket As New PacketClass(OPCODES.PACKET_IDLE)
            Send(klPacket)
        End Sub
    End Class


End Module
