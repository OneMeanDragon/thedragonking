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

Public Enum STATE_FLAGS
    HUB_AUTHORIZED = &H1
    STATUS_INITALIZED = &H2
    FLAG_CHANGED = &H4
    ACCOUNT_AUTHORIZED = &H8
    ACCOUNT_LOGGED_IN = &H10
    ACCOUNT_CYCLEING = &H20
End Enum
Public Enum SERVER_VERSION
    DEFAULT_RV = 0 'Default COMMUNICATION_VERSION
    REVISION_1 = 1 'Version 1 supports all packets 0x00 through 0x0b.
    REVISION_2 = 2 'Version 2 supports messages 0x0c And 0x0d.
    REVISION_4 = 4 'Version 4 supports message 0xa to server.
End Enum
Public Enum CHAT_DROP_OPTIONS_SETTINGS
    ALLOW_ALL = 0
    REFUSE_FROM_NOACCOUNT = 1
    REFUSE_ALL = 2
End Enum
Public Structure CHAT_DROP_OPTIONS_STRUCT
    Public BroadCastedChat As CHAT_DROP_OPTIONS_SETTINGS 
    Public DatabaseChat As CHAT_DROP_OPTIONS_SETTINGS
    Public WhisperChat As CHAT_DROP_OPTIONS_SETTINGS
    Public OtherDatabaseChat As CHAT_DROP_OPTIONS_SETTINGS
End Structure

#Region "Client friendly module"
Public Module cliFunctions
    Friend Function COMS_VER(ByVal requested_version As UInt32) As SERVER_VERSION
        Select Case requested_version
            Case SERVER_VERSION.REVISION_1
                Return SERVER_VERSION.REVISION_1
            Case SERVER_VERSION.REVISION_2
                Return SERVER_VERSION.REVISION_2
            Case SERVER_VERSION.REVISION_4
                Return SERVER_VERSION.REVISION_4
        End Select
        Return SERVER_VERSION.DEFAULT_RV
    End Function
End Module
#End Region

Public Module ClientData
    Public CLIENTs As New Dictionary(Of UInteger, ClientClass) 'TODO: [N/A] Use the clients socket handle as their index. (Apparently I changed this prior to the 2011 builds go me.) 'CLIENTs.ContainsKey(index)

    Public CLIETNIDs As Long = 0

    Public MAXLENGTH_HUB_ID As Integer = 32
    Public MAXLENGTH_HUB_PASSWORD As Integer = 64
    Public MAXLENGTH_BATTLENET_NAME As Integer = 20
    Public MAXLENGTH_BATTLENET_CHANNEL As Integer = 36
    Public MAXLENGTH_DATABASE_ID As Integer = 96
    Public MAXLENGTH_COMMAND As Integer = 384
    Public MAXLENGTH_SENDER As Integer = 32

    Public Structure HostInfos
        Public IP As String         '
        Public HostName As String   '
    End Structure
    Public Structure SocketInfos
        Public sckClient As Socket  '
        Public IP As IPAddress      '
        Public Port As UInteger     '
        Public Host As HostInfos    '
    End Structure

    Public Structure AccountStore
        Public Name As String
        '<password not needed> server itself dosent need to ever store this value, since the server checks validity on the fly.
        Public Flag As UInt32
    End Structure
    Public Structure BattleNStore
        Public Name As String
        Public Channel As String
        Public Server As UInt32
    End Structure
    Public Structure BotNetStorage
        Public ID As UInt32
        Public STATE As UInt32
        Public CLIENT_CAPABILITIES As UInt32
        Public COMMUNICATION_VERSION As SERVER_VERSION
        'Public HUB As AccountStore <not needed>
        Public Account As AccountStore
        Public Database As AccountStore
        Public UserStatus As BattleNStore
    End Structure

    <Serializable()>
    Public Class ClientInfo
        Public Index As UInteger
        '                                           '   _____________
        Public IP As Net.IPAddress                  '<-| IP and PORT |
        Public Port As UInteger                     '<-|_____________|
        Public Account As String = ""                       'TODO: Build Struct to store the minimal needed data, Move it to the client class itself.
        Public AccountFlag As UInt32 = 0                      '
        Public HUB_ID As String = ""                        '<-- Same as account pass note
        Public HUB_PASSWORD As String = ""                  '<-- Same as account pass note
        'Public HUB_FLAG As Int32 = 0                       'Again see note for Flags.... 'Unless I had other intentions with this leaving it here commented out just incase I remember.

        Public Password As String = ""                      '<-- This dosent need to be stored in class, as its only checked at account login and password change level
        Public BattleNetName As String = ""                 '
        Public BattleNetChannel As String = ""              '
        Public BattleNetIP As UInt32 = 0                      '

        Public DatabaseAccountID As String = ""             '
        Public DatabaseAccountPass As String = ""           '
        Public DatabaseFlags As UInt32 = 0                    '
        Public AccountUniqueID As UInt32 = 0                'Users ID on botnet.
        Public ChatDropOptions As CHAT_DROP_OPTIONS_STRUCT  '
        'Public ChatDrop As Integer     'To far ahead of myself < will be in the state flag >
        'Public Flags As UInt32 = 0       'I must have originally intended this as AccountFlag

        '########### Client State ############
        Public STATE As UInt32 = 0          '#  'Required in Login process. for ease of removing bool mess.
        '#####################################

        Public CLIENT_CAPABILITIES As UInt32 = 0
        Public COMMUNICATION_VERSION As SERVER_VERSION = SERVER_VERSION.DEFAULT_RV
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
        Private Function GetUniqueID() As UInt32
            Dim IsUnique As Boolean = False
            Dim TmpID As UInt32 = 1
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
            'NOTE: The above loops can be done in 1 loop. i = 1 to client count + 1, clients.containskey(i)
            Return 0 'Since we cant unsigned a -1 and we wont ever have a client at ID = 0
        End Function

        Private Sub InitLocVars()
            'set default chatdrops
            Me.ChatDropOptions.BroadCastedChat = CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL
            Me.ChatDropOptions.DatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL
            Me.ChatDropOptions.WhisperChat = CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL
            Me.ChatDropOptions.OtherDatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL

        End Sub
        Public Sub OnConnect(ByVal state As Object)
            Me.IP = CType(Socket.RemoteEndPoint, IPEndPoint).Address
            Me.Port = CType(Socket.RemoteEndPoint, IPEndPoint).Port
            'Me.AccountUniqueID = GetUniqueID() 'NOTE: We dont need to do this it can be done via Me.Index.
            Debug.Print("Incomming connection from [" & IP.ToString & ":" & Port & "]")

            Me.Index = Interlocked.Increment(CLIETNIDs)
            Me.AccountUniqueID = Me.Index 'Testing

            Me.InitLocVars()

            Me.Socket.BeginReceive(SocketBuffer, 0, SocketBuffer.Length, SocketFlags.None, AddressOf OnData, Nothing)


            SyncLock CType(CLIENTs, ICollection).SyncRoot
                CLIENTs.Add(Me.Index, Me)
            End SyncLock

            ConnectionsIncrement()
        End Sub

        Private SocketLoopData() As Byte
        Private SocketLoopDataLength As Integer
        Public Sub OnData(ByVal ar As IAsyncResult)
            If Socket Is Nothing Then Return
            If Not Socket.Connected Then Return
            If WS.m_flagStopListen Then Return
            Dim PacketLen As Integer

            Try
                SocketBytes = Socket.EndReceive(ar)
                If SocketBytes = 0 Then
                    Me.Dispose()
                Else
                    Interlocked.Add(DataTransferIn, SocketBytes)

                    ReDim Preserve SocketLoopData(SocketLoopDataLength + SocketBytes) 'need to keep track of them incomplete packets
                    'Copy the socket data into the socket loop data
                    Array.Copy(SocketBuffer, 0, SocketLoopData, SocketLoopDataLength, SocketBytes)
                    SocketLoopDataLength += SocketBytes 'add the socket length to the loop data length
                    While SocketLoopDataLength > 0
                        If SocketLoopDataLength >= 4 Then
                            PacketLen = (SocketLoopData(2) + SocketLoopData(3) * 256)
                        Else
                            PacketLen = 0 'this means we have an incomplete packet in the buffer more data should be incoming.
                        End If
                        If PacketLen > 0 Then 'put all looping data in here
                            'Move packet to Data
                            Dim data(PacketLen - 1) As Byte
                            Array.Copy(SocketLoopData, data, PacketLen)
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
                            SocketLoopDataLength -= PacketLen
                            Array.Copy(SocketLoopData, PacketLen, SocketLoopData, 0, SocketLoopDataLength)
                        Else
                            'do nothing continue recieveing
                        End If
                    End While

                    Socket.BeginReceive(SocketBuffer, 0, SocketBuffer.Length, SocketFlags.None, AddressOf OnData, Nothing)

                    ThreadPool.QueueUserWorkItem(AddressOf OnPacket)
                End If

            Catch sckerr As SocketException
                Select Case sckerr.NativeErrorCode
                    Case 10054 'Connection was closed at remote end point. (Remote Peer)
#If DEBUG Then
                        Debug.Print("WARNING, [" & IP.ToString & ":" & Port & "] " & sckerr.Message)
#End If
                        Exit Select
                End Select
                Me.Dispose()
                Exit Try

            Catch err As Exception
#If DEBUG Then
                Debug.Print("WARNING, [" & IP.ToString & ":" & Port & "] cause error " & err.ToString)
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
                Debug.Print(Me.AccountUniqueID.ToString() & ":" & "bad socket?")
            End If
            If Not Socket.Connected Then
                Delete() 'remove the client
                Exit Sub
            End If

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
            If ((Me.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                    If cliTemp.Value.AccountUniqueID = AccountUniqueID Then
                        'do nothing do not send to the user logging off
                    Else
                        'notify everyone else user logged off
                        If ((cliTemp.Value.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then 'only if they are loggedin
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
            Try
                Me.KeepAliveTimer.Enabled = False
                Me.Dispose()
            Catch Ex As Exception
                MessageBox.Show(Ex.ToString, "Error, void Client.Delete() { .. }")
            End Try
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

            PacketHandlers(OPCODES.PACKET_IDLE) = CType(AddressOf CMSG_NULL, HandlePacket)                                  '       [FULL SUPPORT]          [bp: y]
            PacketHandlers(OPCODES.PACKET_LOGON) = CType(AddressOf CMSG_PACKET_LOGON, HandlePacket)                         '       [FULL SUPPORT 4-8-2018] [bp: y]
            PacketHandlers(OPCODES.PACKET_STATSUPDATE) = CType(AddressOf CMSG_PACKET_STATSUPDATE, HandlePacket)             '       [MIN SUPPORT 4-8-2018]  [bp: y] <cycleing not imp> <cycleing considered defunct anyhow>
            PacketHandlers(OPCODES.PACKET_DATABASE) = CType(AddressOf CMSG_PACKET_DATABASE, HandlePacket)                   '[Not implemented]
            PacketHandlers(OPCODES.PACKET_MESSAGE) = CType(AddressOf CMSG_PACKET_MESSAGE, HandlePacket)                     '       [MIN SUPPORT 4-11-2018] [bp: y] <malformed sender check not imp>
            PacketHandlers(OPCODES.PACKET_CYCLE) = CType(AddressOf CMSG_PACKET_CYCLE, HandlePacket)                         '[Not implemented]
            PacketHandlers(OPCODES.PACKET_USERINFO) = CType(AddressOf CMSG_PACKET_USERINFO, HandlePacket)                   '       [FULL SUPPORT 4-8-2018] [bp: y]
            PacketHandlers(OPCODES.PACKET_BROADCASTMESSAGE) = CType(AddressOf CMSG_PACKET_BROADCASTMESSAGE, HandlePacket)   '       [MIN SUPPORT 4-11-2018] [bp: y] <malformed sender check not imp>
            PacketHandlers(OPCODES.PACKET_COMMAND) = CType(AddressOf CMSG_PACKET_COMMAND, HandlePacket)                     '       [MIN SUPPORT 4-11-2018] [bp: y] <malformed sender check not imp>
            PacketHandlers(OPCODES.PACKET_CHANGEDBPASSWORD) = CType(AddressOf CMSG_PACKET_CHANGEDBPASSWORD, HandlePacket)   '[Not implemented]
            PacketHandlers(OPCODES.PACKET_BOTNETVERSION) = CType(AddressOf CMSG_PACKET_BOTNETVERSION, HandlePacket)         '       [MIN SUPPORT 4-8-2018]  [bp: y] <client capabilitys dword not imp>
            PacketHandlers(OPCODES.PACKET_BOTNETCHAT) = CType(AddressOf CMSG_PACKET_BOTNETCHAT, HandlePacket)               '       [FULL SUPPORT 4-8-2018] [bp: y]
            PacketHandlers(OPCODES.PACKET_ADMIN) = CType(AddressOf CMSG_BOTNET_ADMIN, HandlePacket)                         '[Not implemented]
            PacketHandlers(OPCODES.PACKET_ACCOUNT) = CType(AddressOf CMSG_PACKET_ACCOUNT, HandlePacket)                     '       [FULL SUPPORT 4-6-2018] [bp: y]
            PacketHandlers(OPCODES.PACKET_CHATDROPOPTIONS) = CType(AddressOf CMSG_CHAT_DROP, HandlePacket)                  '       [FULL SUPPORT 4-8-2018] [bp: y] <unless it extended to the command processor aswell>
        End Sub
        'BotData.BotNet.DataBaseName = .GetString("Database", "MiniCounsel")
        'BotData.BotNet.DataBasePassword = .GetString("Password", "65db65ca4e8d7f5a")
        Public Sub CMSG_BOTNET_ADMIN(ByRef packet As PacketClass, ByRef Client As ClientClass)

        End Sub
        Public Sub CMSG_PACKET_CHANGEDBPASSWORD(ByRef packet As PacketClass, ByRef Client As ClientClass)
            '(UINT32) Password selection
            '   0x00: Read-only
            '   0x01: Full
            '   0x02: Restricted
            '(STRING) New password 
        End Sub
        Public Sub CMSG_PACKET_DATABASE(ByRef packet As PacketClass, ByRef Client As ClientClass)
            '(UINT32) Subcommand
            '
            'For subcommand 0x01 (Transfer status):   [rev: 4]
            '    (UINT32) Transfer event
            'For subcommand 0x02 (Entry modified)
            '    (UINT32) Sending Bot ID              [rev 4; awl: 1]
            '    (UINT32) Last modification time      [rev: 4; awl: 1]
            '    (STRING) Usermask
            '    (STRING) Flags
            '    (STRING) Comment                     [rev: 4; awl: 1]
            'For subcommand 0x03 (Entry removed):
            '   (UINT32) Sending Bot ID              [rev 4; awl: 1]
            '   (UINT32) Last modification time      [rev: 4; awl: 1]
            '   (STRING) Usermask
            '   (STRING) Comment                     [rev: 4; awl: 1]        
        End Sub
        Public Sub CMSG_PACKET_CYCLE(ByRef packet As PacketClass, ByRef Client As ClientClass) '[DEFUNCT] 
            '(UINT32)   Count
            '(STRING)[] Usernames           
            '
            'Send cycle request (defunct).
            'With the changes Battle.net made To operators In Private channels, this message Is considered defunct.
            'The Usernames String array should contain the number Of entries specified In Count, specifying the usernames of bots to cycle.

            If (packet.Length < 9) Or (packet.Length > 512) Then 'Bad packet (min: 9, max: 512) [512 as i remember is the maximum sized packet that can be sent]
                Call Client.Delete()
                Return
            End If
            Return 'They can send it but does nothing.

            Dim NumberOfNames As UInt32 = packet.GetInt32()
            'We can test the packet size once more before going through the listing
            If Not (packet.Length >= (NumberOfNames + 8)) Then 'if the lengths are = then the names are all empty strings.
                'NumberOfNames = at the minimum how many nulls are in this feild and 8 is the head + count dwords.
                Call Client.Delete()
                Return
            End If

            Dim dwOffset As UInt32 = 8
            Dim Names(NumberOfNames) As String
            Dim i As UInt32 = 0
            For i = 0 To NumberOfNames
                If Not (dwOffset < packet.Length) Then
                    'If its not less than, then its equal or greater meaning we are going to go passed the end of the packet
                    Call Client.Delete()
                    Return
                End If
                Names(i) = packet.GetString()
                dwOffset += Names(i).Length + 1
                'Should check if Names(i) = ""
                'Should check if Names(i).length > bnutName.MaxLength
            Next
            'Final packet check
            If Not (packet.Length = (dwOffset + 8)) Then
                'Both values should match else bad packet
                Call Client.Delete()
                Return
            End If

            'Now that we got the names do something.

        End Sub

        Public Sub OnUnhandledPacket(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Debug.Print("LogType.WARNING, [" & Client.IP.ToString & ":" & Client.Port.ToString & "] " & CType(packet.OpCode, OPCODES) & " [Unhandled Packet]" & " [Protocol version: " & packet.ProtocalVersion & "]")
        End Sub
#Region "Response Enumerators"
        Private Enum STATS_UPDATE
            FAIL = 0
            ACCEPTED = 1
        End Enum
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
        Private Enum PACKET_COMMAND_ACTIONS
            TALK = 0
            EMOTE = 1
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
            DATABASE_INFO_DID_NOT_MATCH = 14
            BAD_STATE = 15
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
            ACCOUNT_CREATION_FAIL = 9
            ACCOUNT_CREATION_ACCOUNT_EXISTS = 10
            USERNAME_BAD_LENGTH = 11
            PASSWORD_BAD_LENGTH = 12
            PACKET_BAD_LENGTH = 13
            BAD_PACKET = 14
            ACCOUNT_CHANGEPASSWORD_ACCOUNTNONEXISTANT = 15
            ACCOUNT_CHANGEPASSWORD_BADPASSWORD = 16
            ACCOUNT_CHANGEPASSWORD_REQUIRESSERVERADMINHELP = 17
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


#Region "Completed Packets"
        Private Sub PROTOCOL_VIOLATION(ByRef Client As ClientClass, ByVal ErrorFrom As OPCODES, ByVal ErrorID As Integer, ByVal pLength As UInt32, ByVal pRemainingLen As UInt32)
            Debug.Print("PROTOCOL_VIOLATION" & vbNewLine)
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


        Private Sub SEND_PACKET_USERINFO(ByRef ToClient As ClientClass, ByVal FromClient As ClientClass)
            Debug.Print("SEND_PACKET_USERINFO" & vbNewLine)
            Dim response As New PacketClass(OPCODES.PACKET_USERINFO)
            response.AddInt32(FromClient.AccountUniqueID)
            If ToClient.COMMUNICATION_VERSION = SERVER_VERSION.REVISION_1 Then
                response.AddInt32(FromClient.DatabaseFlags)
                response.AddInt32(FromClient.AccountFlag) 'Added
                If ((ToClient.AccountFlag And Functions.FLAGS.A) = Functions.FLAGS.A) Or ((ToClient.AccountFlag And Functions.FLAGS.L) = Functions.FLAGS.L) Then
                    response.AddByteArray(FromClient.IP.GetAddressBytes())
                    'response.AddByteArray(CType(FromClient.Socket.RemoteEndPoint, IPEndPoint).Address.GetAddressBytes())
                End If
            End If
            response.AddString(FromClient.BattleNetName)
            response.AddString(FromClient.BattleNetChannel)
            response.AddInt32(FromClient.BattleNetIP)
            response.AddString(FromClient.Account)
            response.AddString(FromClient.DatabaseAccountID)
            ToClient.Send(response)
        End Sub
        Public Sub CMSG_PACKET_USERINFO(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Debug.Print("CMSG_PACKET_USERINFO" & vbNewLine)
            If (packet.Length < 4) Or (packet.Length > 4) Then '(min: 4, max: 4) [done]
                Call Client.Delete()
                Return
            End If

            If ((Client.STATE And STATE_FLAGS.ACCOUNT_AUTHORIZED) = STATE_FLAGS.ACCOUNT_AUTHORIZED) Then
                If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                    'Log them in send everyone else the update
                    If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Or ((Client.STATE And STATE_FLAGS.FLAG_CHANGED) = STATE_FLAGS.FLAG_CHANGED) Then
                        SEND_PACKET_STATSUPDATE(Client, 1) '1 = sucess.
                    End If

                    'Double login?
                    If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                        Client.STATE += STATE_FLAGS.ACCOUNT_LOGGED_IN 'Update they have logged in finnaly
                    End If

                    'Dim start_of_userlisting As New PacketClass(OPCODES.PACKET_USERINFO)
                    'Client.Send(start_of_userlisting) 'Why the voided packet at the start fuck sake.

                    For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                        If ((cliTemp.Value.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) = False Then
                            'do nothing do not send to the user logging in
                        Else
#Region "Botnet Notes"
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
#End Region
                            SEND_PACKET_USERINFO(cliTemp.Value, Client)
                        End If
                    Next
                    'log client in, send client user details of those that are on the server.
                    'Client.LoggedIn = True
                    'SEND_PACKET_USERINFO(Client, Client) 'the above loop should do this now

                    For Each cliTemp2 As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                        If ((cliTemp2.Value.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                            If Not cliTemp2.Value.AccountUniqueID = Client.AccountUniqueID Then
                                SEND_PACKET_USERINFO(Client, cliTemp2.Value)
                            End If
                        End If
                    Next
                    'Question here, is this sent everytime this list is requested or just on the inital server join.
                    Dim end_of_userlisting As New PacketClass(OPCODES.PACKET_USERINFO)
                    Client.Send(end_of_userlisting)
                Else
                    '
                End If
            Else
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_USERINFO, PROTOCOL_VIOLATION_COMMAND_6.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Debug.Print("CMSG_PACKET_USERINFO" & vbNewLine)
        End Sub

        Public Sub SMSG_PACKET_BOTNETVERSION(ByRef Client As ClientClass)
            Debug.Print("SMSG_PACKET_BOTNETVERSION" & vbNewLine)
            Dim response As New PacketClass(OPCODES.PACKET_BOTNETVERSION)
            response.AddInt32(SERVER_VERSION.REVISION_4) '4.0 0->1 4.1
            Client.Send(response)
        End Sub
        Public Sub SMSG_PACKET_BOTNETSUBVERSION(ByRef Client As ClientClass)
            Debug.Print("SMSG_PACKET_BOTNETSUBVERSION" & vbNewLine)
            Dim response As New PacketClass(OPCODES.PACKET_BOTNETVERSION_AK)
            response.AddInt32(Client.COMMUNICATION_VERSION) '4.0 0->1 4.1
            Client.Send(response)
        End Sub
        Public Sub CMSG_PACKET_BOTNETVERSION(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Debug.Print("CMSG_PACKET_BOTNETVERSION" & vbNewLine)
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

            '[Violation codes]
            '    Command 10
            '2 = missing first dword
            '3 = missing second dword
            '4 = unexpected flags in second dword [dont know how this works exactly...]
            '   PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 2, packet.Length, (packet.Length - packet.Offset))
            If packet.Length <= 7 Or packet.Length > 12 Then 'Bad packet (min: 8 max: 12)
                Call Client.Delete()
                Return
            End If
            If packet.Length > 4 And packet.Length < 8 Then
                '2 = missing first dword
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 2, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If packet.Length > 8 And packet.Length < 12 Then
                '3 = missing second dword
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 3, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            If packet.Length >= 8 Then 'data after the header
                Dim tmpComunVer As UInt32 = packet.GetInt32
                Client.COMMUNICATION_VERSION = COMS_VER(tmpComunVer)
                'Else Client.COMMUNICATION_VERSION = SERVER_VERSION.DEFAULT_RV 'this is here as a marker, the server by default is SERVER_VERSION.DEFAULT_RV
            End If
            If packet.Length >= 12 Then
                Client.CLIENT_CAPABILITIES = packet.GetInt32
            End If

            '(send to client) id 0x09: acknowledge communication version
            'Contents:
            '	(DWORD) communication version.  This message is sent to confirm
            '	acceptance of msg 0x0a.  All subsequent messages will be formed in
            '	this style.  That is, clients should not change parsing methods until
            '	the server confirms the new style.
            'Response:   None()
            SMSG_PACKET_BOTNETSUBVERSION(Client)
        End Sub

        Private Sub SEND_PACKET_STATSUPDATE(ByRef ThisClient As ClientClass, ByVal iResponse As UInt32)
            Debug.Print("SEND_PACKET_STATSUPDATE" & vbNewLine)
            Dim response As New PacketClass(OPCODES.PACKET_STATSUPDATE)
            response.AddInt32(iResponse)
            If ((ThisClient.STATE And STATE_FLAGS.FLAG_CHANGED) = STATE_FLAGS.FLAG_CHANGED) Then
                ThisClient.STATE -= STATE_FLAGS.FLAG_CHANGED 'send update and remove flag
                response.AddInt32(ThisClient.AccountFlag) 'Only way this makes sense
            End If
            ThisClient.Send(response)
        End Sub
        Public Sub CMSG_PACKET_STATSUPDATE(ByRef packet As PacketClass, ByRef Client As ClientClass) '0x02
            Debug.Print("CMSG_PACKET_STATSUPDATE" & vbNewLine)

            '(send to server) id 0x02 update bot stats
            'Contents:
            '(STRING20) unique username on battle.net : This String provides room
            '	to specify a name mangle ("Someone#2"), but Not to specify
            '           gateway.
            '(String: 36) current channel on battle.net : By convention,
            '    "<Not logged on>" Is used to indicate clients which are Not
            '        presently connected.
            '(DWORD) battle.net server ip
            '(STRING:96) database id (which database to use) : includes database
            '        Password.Use the following format:  "name password\0".
            '(DWORD) cycle status : 0=NotCycling, 1=Cycling

            '    Command 2                          '(min: 15, max: 164)    [done]
            ' 2 = bad state, client has Not authenticated with command 1    [done]
            ' 3 = packet too small                                          [nixed] <- this is handled at the head of the parser
            ' 4 = bad size for BNCSName                                     [done]
            ' 5 = channel                                                   [done]
            ' 6 = missing server IP                                         [done]
            ' 7 = bad size for database string                              []
            ' 8 = missing cycle status                                      []
            ' 9 = empty BNCSName                                            [done]
            '10 = empty channel                                             [done]
            '11 = bad server IP                                             []
            '12 = badly formed BNCSName                                     []
            '13 = badly formed database string                              []

            If packet.Length < 15 Or packet.Length > 164 Then 'Bad packet (min: 15, max: 164)
                Call Client.Delete()
                Return
            End If

            If Not ((Client.STATE And STATE_FLAGS.HUB_AUTHORIZED) = STATE_FLAGS.HUB_AUTHORIZED) Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.CLIENT_NOTYET_AUTHORIZED, packet.Length, (packet.Length - packet.Offset))
                Return
                'ElseIf Not ((Client.STATE And STATE_FLAGS.ACCOUNT_AUTHORIZED) = STATE_FLAGS.ACCOUNT_AUTHORIZED) Then
                '    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.CLIENT_NOTYET_AUTHORIZED, packet.Length, (packet.Length - packet.Offset))
                '    Return
            End If

            Dim dwOffsetLoc As UInt32 = 4
            If Not (packet.Length > dwOffsetLoc) Then 'Missing bnut name ()
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.LEN_PACKET_TO_SMALL, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Client.BattleNetName = packet.GetString
            dwOffsetLoc += Client.BattleNetName.Length + 1
            If Client.BattleNetName.Length > MAXLENGTH_BATTLENET_NAME Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.LEN_VIOLATES_BATTLENET_NAME_LENGTH, packet.Length, packet.Offset)
                Return
            ElseIf Client.BattleNetName = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BLANK_BATTLENET_NAME_EMPTY, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Client.BattleNetChannel = packet.GetString
            dwOffsetLoc += Client.BattleNetChannel.Length + 1
            If Client.BattleNetChannel.Length > MAXLENGTH_BATTLENET_CHANNEL Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.LEN_VIOLATES_CHANNEL_NAME_LENGTH, packet.Length, packet.Offset)
                Return
            ElseIf Client.BattleNetChannel = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BLANK_BATTLENET_CHANNELNAME_EMPTY, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            If (Not (packet.Length > dwOffsetLoc)) And (Not (packet.Length > (dwOffsetLoc + 4))) Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.MISSING_SERVER_IP, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Client.BattleNetIP = packet.GetInt32
            dwOffsetLoc += 4

            If (Not (packet.Length > dwOffsetLoc)) Then
                'bad packet
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.LEN_PACKET_TO_SMALL, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim tmpStr As String = packet.GetString 'Database tempt holder.
            If tmpStr.Length < 3 Or tmpStr.Length > MAXLENGTH_DATABASE_ID Then
                ' 7 = bad size for database string 
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BAD_DATABASE_SIZE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            dwOffsetLoc += tmpStr.Length + 1

            Dim DatabaseData() As String = tmpStr.Split(" ")
            Client.DatabaseAccountID = DatabaseData(0)
            Client.DatabaseAccountPass = tmpStr.Substring(Client.DatabaseAccountID.Length + 1) 'DatabaseData(1) 'System.Text.Encoding.UTF8.GetBytes(tmpStr.Split(" ")(1))

            If Client.DatabaseAccountID = "" Or Client.DatabaseAccountPass = "" Then
                ' 7 = bad size for database string [Missing strings]
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BAD_DATABASE_SIZE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            If (Not (packet.Length > dwOffsetLoc)) And (Not (packet.Length > (dwOffsetLoc + 4))) Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.LEN_PACKET_TO_SMALL, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim IsCycleing As Integer = packet.GetInt32()
            If IsCycleing > 0 Then
                IsCycleing = 1
            Else
                IsCycleing = 0
            End If

            If IsCycleing > 0 Then
                If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_CYCLEING) = STATE_FLAGS.ACCOUNT_CYCLEING) Then 'just incase.....
                    Client.STATE += STATE_FLAGS.ACCOUNT_CYCLEING
                End If
            Else
                If ((Client.STATE And STATE_FLAGS.ACCOUNT_CYCLEING) = STATE_FLAGS.ACCOUNT_CYCLEING) Then 'just incase.....
                    Client.STATE -= STATE_FLAGS.ACCOUNT_CYCLEING
                End If
            End If

            'Check if the database info matches if not then fail.
            Dim iResponse As UInt32 = 0
            If Client.DatabaseAccountPass = GetDatabasePass(Client.DatabaseAccountID) Then
                Client.DatabaseFlags = GetDatabaseFlags(Client.DatabaseAccountID, Client.Account)
                If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                    'This user is not logged in yet send once the userlist requested, so that they get their account flag.
                    'If we got here the user has passed this section set the state and continue.
                    If ((Client.STATE And STATE_FLAGS.STATUS_INITALIZED) = STATE_FLAGS.STATUS_INITALIZED) Then
                        'Client has already initalized their request, shouldent get here but just incase we do.
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.BAD_STATE, packet.Length, packet.Offset)
                        Return
                    Else
                        'dont need to test this if we dont get here the client selfdestructs.
                        Client.STATE += STATE_FLAGS.STATUS_INITALIZED
                    End If
                    Return
                End If

                iResponse = STATS_UPDATE.ACCEPTED
            Else
                '#################################
                'Failed to login to Database    '#  'removed STATS_UPDATE.Failed response.
                '#################################
                If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then 'if this is true they are on the userlist already
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_STATSUPDATE, PROTOCOL_VIOLATION_COMMAND_2.DATABASE_INFO_DID_NOT_MATCH, packet.Length, (packet.Offset - 4)) '-4 = go back before the cycling dword
                    Return
                End If
            End If

            SEND_PACKET_STATSUPDATE(Client, iResponse)

            Debug.Print("CMSG_PACKET_STATSUPDATE" & vbNewLine)
            For Each cliTemp2 As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If ((cliTemp2.Value.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then 'if this user is logged in and is cycling
                    If Not cliTemp2.Value.AccountUniqueID = Client.AccountUniqueID Then
                        SEND_PACKET_USERINFO(cliTemp2.Value, Client)
                    End If
                End If
            Next
        End Sub

        'Updated for Version #4.2 'Added violation sends for fuckups.
        Public Sub CMSG_PACKET_LOGON(ByRef packet As PacketClass, ByRef Client As ClientClass) '0x01
            'contents: (min: 6, max: 100) [done]
            'string[32] hubid
            'string[64] hubpassword
            '
            '    Command 1 [protocol errors]
            '2 = bad state, client Is attempting to authenticate a second time  [done]
            '3 = bad size for hub ID                                            [done]
            '4 = bad size for hub password                                      [done]
            '5 = empty hub ID                                                   [done]
            '6 = empty hub password                                             [done]

            'Packet size etc. 
            If packet.Length < 6 Or packet.Length > 100 Then 'min-max above in notes
                Call Client.Delete()
                Return
            End If

            If ((Client.STATE And STATE_FLAGS.HUB_AUTHORIZED) = STATE_FLAGS.HUB_AUTHORIZED) Then 'Tryed to logon again after allready loging on
                '2 = bad state, client Is attempting to authenticate a second time
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.CLIENT_ATTEMPTED_AUTHING_A_SECOND_TIME, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Client.HUB_ID = packet.GetString

            If Client.HUB_ID.Length > 31 Then 'Null not included when your testing strings so its 31 instead of 32 (TODO: Switch this to GetByteString)
                '3 = bad size for hub ID 
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.BAD_STRLEN_HUBID, packet.Length, packet.Offset)
                Return
            End If

            Client.HUB_PASSWORD = packet.GetString

            If Client.HUB_PASSWORD.Length > 63 Then 'Null not included when your testing strings so its 63 instead of 64 (TODO: Switch this to GetByteString)
                '4 = bad size for hub password 
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.BAD_STRLEN_HUB_PASSWORD, packet.Length, packet.Offset)
                Return
            End If

            If Client.HUB_ID = "" Then
                '5 = empty hub ID      
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.STR_BLANK_HUBID, packet.Length, packet.Offset)
                Return
            End If
            If Client.HUB_PASSWORD = "" Then
                '6 = empty hub password      
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_LOGON, PROTOCOL_VIOLATION_COMMAND_1.STR_BLANK_HUB_PASSWORD, packet.Length, packet.Offset)
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

            'Why is this a forced packet here, forced to send 0xA for sphtbot.
            SMSG_PACKET_BOTNETVERSION(Client)

            Dim response As New PacketClass(OPCODES.PACKET_LOGON)
            response.AddInt32(HUB_RESPONCE)
            Client.STATE += STATE_FLAGS.HUB_AUTHORIZED 'If the users password is accepted then

            'If packet.ProtocalVersion >= 4.2 Then                     'Version 4.2
            'If Client.
            'Dim ThisIP() As Byte = Client.IP.GetAddressBytes
            '    response.AddInt8(Client.IP.GetAddressBytes.Length)  'IPv4|IPv6 Packet Update
            'Else                                                    'this will only send to users version 4.2+
            'End If
            response.AddByteArray(Client.IP.GetAddressBytes)
            Client.Send(response)
            Debug.Print("CMSG_PACKET_LOGON" & vbNewLine)
        End Sub

        Public Sub CMSG_NULL(ByRef packet As PacketClass, ByRef Client As ClientClass)
            If (packet.Length < 4) Or (packet.Length > 4) Then
                Call Client.Delete() 'Bad packet death to user.
                Return
            End If
            'Else we do not respond to this packet
        End Sub

#Region "---- 0x0D PACKET_ACCOUNT (account management) [done] ----"
        Private Sub VoidUserAccount(ByVal atIndex As UInt32)
            Dim tClient As ClientClass = CLIENTs.Item(atIndex)
            tClient.Account = ""

            'Update these users, if the account was logged on.
            If (tClient.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN Then
                For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                    If (cliTemp.Value.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN Then 'Not (cliTemp.Value.Index = atIndex) Then
                        SEND_PACKET_USERINFO(cliTemp.Value, tClient)
                    End If
                Next
            End If
        End Sub
        Public Sub SMSG_PACKET_ACCOUNTLI(ByRef client As ClientClass, ByVal res As UInt32, ByVal AccountName As String)
            Debug.Print("SMSG_PACKET_ACCOUNTLI" & vbNewLine)
            'Update users flag.

            Dim response As New PacketClass(OPCODES.PACKET_ACCOUNT)
            response.AddInt32(PACKET_ACCOUNT_COMMANDS.LOGIN)
            response.AddInt32(res)
            client.Send(response)

            If Not (res = PACKET_ACCOUNT_RESULTS.FAILED) Then
                'Check if account name is currently online, if it is void it and update the that client.
                Dim tIndex As UInt32 = GetUserIndex(AccountName, client.Index)
                If Not (tIndex = 0) Then
                    'theres an account using our name void them
                    VoidUserAccount(tIndex)
                    Return
                End If
                If ((client.STATE And STATE_FLAGS.FLAG_CHANGED) = STATE_FLAGS.FLAG_CHANGED) Then
                    SEND_PACKET_STATSUPDATE(client, 1) 'Update this clients flag now.
                    Return
                End If
            End If
        End Sub
        Public Sub CMSG_PACKET_ACCOUNT(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Debug.Print("CMSG_PACKET_ACCOUNT" & vbNewLine)

            '(send to server) id 0x0d Account Management
            'Contents: (min: 11, max: 216)
            '	(DWORD) subcommand
            '	(STRING:16) account name	: name to acquire
            '	* Subcommand 0x00: Login : 
            '		(STRING96) account password
            '	* Subcommand 0x01: Change Password: 
            '		(STRING96) old password
            '		(STRING:96) New password
            '	* Subcommand 0x02: Account create:  
            '       (STRING96) account password
            'contents:
            'dword command
            'string[16] account name
            'string[96]
            '   if command = 1 then
            '       string[96]
            '
            'Check for bad packet size.
            If packet.Length <= 11 Or packet.Length > 216 Then 'Bad packet (min: 11, max: 216)
                Call Client.Delete()
                Return
            End If

            '    command 13 [protocol violations]
            '1 = missing subcommand                         [done]
            '2 = bad subcommand                             [done]
            '3 = missing username                           [done]
            '4 = empty username                             [done]
            '5 = missing password                           [done]
            '6 = empty password                             [done]
            'There are no subcommand 0 specific errors.     -
            'Subcommand 1                                   -
            '7 = missing New password                       [done]
            '8 = empty New password                         [done]
            'There are no subcommand 2 specific errors.     -
            Dim dwOffsetVal As UInt32 = 8
            If packet.Length < dwOffsetVal Then
                '1 = missing subcommand
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.MISSING_SUBCOMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim command As Integer = packet.GetInt32

            If (command < 0) Or (command > 2) Then
                '2 = bad subcommand   
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.BAD_SUBCOMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            If packet.Length < dwOffsetVal Then
                '3 = missing username
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.MISSING_USERNAME, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim accName As String = packet.GetString
            dwOffsetVal += accName.Length + 1

            If accName = "" Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.EMPTY_USERNAME, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If accName.Length > STRING_LENGTHS.ACCOUNT_LENGTH Then
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.USERNAME_BAD_LENGTH, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            If packet.Length < dwOffsetVal Then
                '5 = missing password
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.MISSING_PASSWORD, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            'There is always at the very least 1 password
            Dim accPass As String = packet.GetString
            dwOffsetVal += accPass.Length + 1

            If accPass = "" Then
                '6 = empty password
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.EMPTY_PASSWORD, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If accPass.Length > STRING_LENGTHS.PASSWORD_LENGTH Then 'these bad lengths i have marked are probably drop connections on normal botnet
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.PASSWORD_BAD_LENGTH, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim accPass2 As String = ""
            If command = PACKET_ACCOUNT_COMMANDS.CHANGE_PASSWORD Then
                'then we have the second password to test.
                If packet.Length < dwOffsetVal Then
                    '7 = missing New password 
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.SC1_MISSING_NEWPASSWORD, packet.Length, (packet.Length - packet.Offset))
                    Return
                End If
                accPass2 = packet.GetString
                dwOffsetVal += accPass2.Length + 1
                If accPass2 = "" Then
                    '8 = empty New password 
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.SC1_EMPTY_NEWPASSWORD, packet.Length, (packet.Length - packet.Offset))
                    Return
                End If
                If accPass2.Length > STRING_LENGTHS.PASSWORD_LENGTH Then 'these bad lengths i have marked are probably drop connections on normal botnet
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.PASSWORD_BAD_LENGTH, packet.Length, (packet.Length - packet.Offset))
                    Return
                End If
            End If

            Dim response As New PacketClass(OPCODES.PACKET_ACCOUNT)
            Select Case command
#Region "PACKET_ACCOUNT_COMMANDS.LOGIN [done]"
                Case PACKET_ACCOUNT_COMMANDS.LOGIN
                    Dim UserIsLoggedOn As Boolean = IsUserNameOnLine(accName)
                    'TODO: If a user is online on the same name kick them... theres no reason to have "<No Account>" people running around, seriously make another account for the second login.

                    'response.AddInt32(PACKET_ACCOUNT_COMMANDS.LOGIN)**************************
                    Dim tmpPath As String = AccountsPath & accName & "\"
                    If Not Directory.Exists(tmpPath) Then 'Account dosent exist.
                        'response.AddInt32(PACKET_ACCOUNT_RESULTS.FAILED)
                        SMSG_PACKET_ACCOUNTLI(Client, PACKET_ACCOUNT_RESULTS.FAILED, "")
                    Else 'test password
                        If accPass = GetPassword(tmpPath) Then
                            'response.AddInt32(PACKET_ACCOUNT_RESULTS.PASSED)
                            If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_AUTHORIZED) = STATE_FLAGS.ACCOUNT_AUTHORIZED) Then 'Just incase.....
                                Client.STATE += STATE_FLAGS.ACCOUNT_AUTHORIZED
                                Client.STATE += STATE_FLAGS.FLAG_CHANGED
                            End If
                            Client.Account = accName
                            Client.Password = accPass
                            Client.AccountFlag = GetAccountFlags(AccountsPath & Client.Account)
                            SMSG_PACKET_ACCOUNTLI(Client, PACKET_ACCOUNT_RESULTS.PASSED, Client.Account)
                        Else
                            If ((Client.STATE And STATE_FLAGS.ACCOUNT_AUTHORIZED) = STATE_FLAGS.ACCOUNT_AUTHORIZED) Then 'Just incase.....
                                Client.STATE -= STATE_FLAGS.ACCOUNT_AUTHORIZED
                            End If
                            'response.AddInt32(PACKET_ACCOUNT_RESULTS.FAILED)
                            SMSG_PACKET_ACCOUNTLI(Client, PACKET_ACCOUNT_RESULTS.FAILED, "")
                        End If
                    End If
                    Return
#End Region
#Region "PACKET_ACCOUNT_COMMANDS.CHANGE_PASSWORD [TODO]"
                Case PACKET_ACCOUNT_COMMANDS.CHANGE_PASSWORD
                    '1st Does this account even exist.
                    Dim tmpPath As String = AccountsPath & accName & "\"
                    If Not Directory.Exists(tmpPath) Then 'Account dosent exist.
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.ACCOUNT_CHANGEPASSWORD_ACCOUNTNONEXISTANT, packet.Length, (packet.Length - packet.Offset))
                        Return
                    Else '1: Check if old password matches current password, if it dosent protocol-violation boot
                        If accPass = GetPassword(tmpPath) Then 'PASSWORD match prep new password.
                            '2: Make new password, test if it was changed send accepted message.
                            If accPass2 = NewPassword(tmpPath, accPass2) Then
                                'Passed, else password was not updated, or other error which will require admin help [when admin support is added].
                            Else
                                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.ACCOUNT_CHANGEPASSWORD_REQUIRESSERVERADMINHELP, packet.Length, (packet.Length - packet.Offset))
                                Return
                            End If
                        Else 'Password did not match
                            PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.ACCOUNT_CHANGEPASSWORD_BADPASSWORD, packet.Length, (packet.Length - packet.Offset))
                            Return
                        End If
                    End If

                    response.AddInt32(PACKET_ACCOUNT_COMMANDS.CHANGE_PASSWORD)
                    response.AddInt32(PACKET_ACCOUNT_RESULTS.PASSED) 'From here we should never manage to be a fail.
                    Client.Send(response)
                    Return
#End Region
#Region "PACKET_ACCOUNT_COMMANDS.CREATE_ACCOUNT [TODO: Check for malformed name]"
                Case PACKET_ACCOUNT_COMMANDS.CREATE_ACCOUNT
                    Dim tmpPath As String = AccountsPath & accName & "\"
                    If Directory.Exists(tmpPath) Then
                        PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.ACCOUNT_CREATION_ACCOUNT_EXISTS, packet.Length, (packet.Length - packet.Offset))
                        Return
                    End If

                    response.AddInt32(PACKET_ACCOUNT_COMMANDS.CREATE_ACCOUNT)
                    If Not CreateAccount(tmpPath, accPass) Then 'if the files still failed to create then account creation = fail
                        response.AddInt32(PACKET_ACCOUNT_RESULTS.FAILED)
                        'if Client.COMMUNICATION_VERSION >= 2 then
                        '   response.AddInt32(FailReason)
                        'end if
                    Else
                        response.AddInt32(PACKET_ACCOUNT_RESULTS.PASSED)
                    End If
                    Client.Account = accName
                    Client.Password = accPass
                    Client.Send(response)
                    Return
#End Region
            End Select
            PROTOCOL_VIOLATION(Client, OPCODES.PACKET_ACCOUNT, PROTOCOL_VIOLATION_COMMAND_13.BAD_PACKET, packet.Length, (packet.Length - packet.Offset))
        End Sub
#End Region

#Region " ---- Commands 0x04 Messages [done] ---- "
        Public Sub CMSG_PACKET_BROADCASTMESSAGE(ByRef packet As PacketClass, ByRef Client As ClientClass) 'Command to all bots.
            '		2 = bad state, client tried to issue command while invisible    [done]
            '       3 = missing sender string                                       [done]
            '       4 = missing command                                             [done]
            '       5 = empty sender                                                [done]
            '       6 = empty command                                               [done]
            '       7 = malformed sender name                                       [    ] <- what?
            '                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_STATE, packet.Length, (packet.Length - packet.Offset))

            '(STRING) Sender name    [max: 32]   (min: 6, max: 420)
            '(STRING) Command        [max: 384]
            '
            If (packet.Length < 5) Then
                'missing sender + command
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BROADCASTMESSAGE, PROTOCOL_VIOLATION_COMMAND_7.MISSING_SENDER_SRTING, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If (packet.Length < 6) Or (packet.Length > 420) Then 'Bad packet (min: 6, max: 420)
                Call Client.Delete()
                Return
            End If
            If (Not ((Client.AccountFlag And FLAGS.B) = FLAGS.B)) And (Not ((Client.AccountFlag And FLAGS.A) = FLAGS.A)) Then
                'client must have flag "B" or "A"
                Call Client.Delete()
                Return
            End If
            If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                '2 = bad state, client tried to issue command while invisible 
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BROADCASTMESSAGE, PROTOCOL_VIOLATION_COMMAND_7.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If


            'Dont need to encode these strip from the packet and fire off
            Dim SenderName() As Byte = packet.GetByteString()
            If (SenderName.Length = 1) Then
                'empty sender
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BROADCASTMESSAGE, PROTOCOL_VIOLATION_COMMAND_7.EMPTY_SENDER, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If (packet.Length = (SenderName.Length + 4)) Then
                'missing command
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BROADCASTMESSAGE, PROTOCOL_VIOLATION_COMMAND_7.MISSING_COMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim command() As Byte = packet.GetByteString()
            If (command.Length = 1) Then
                'empty sender
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BROADCASTMESSAGE, PROTOCOL_VIOLATION_COMMAND_7.EMPTY_COMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            'The message goes to every client.
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If ((cliTemp.Value.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                    SMSG_PACKET_COMMANDOVERBOTNET(Client.AccountUniqueID, PACKET_COMMAND_COMMANDS.BROADCAST_TO_ALL_USERS, SenderName, command, cliTemp.Value)
                End If
            Next
        End Sub
        Public Sub CMSG_PACKET_COMMAND(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Debug.Print("CMSG_PACKET_COMMAND" & vbNewLine)

            '(send to server) id 0x08: command to specific botnet bot
            'Contents:
            '	(DWORD) target bot id       (min: 10, max: 424)
            '	(STRING:32) sending user
            '	(STRING:384) command
            'Response:   the server forwards the message to the specified bot in the
            '    Form of a msg 0x4, providing that
            '1) The target Is online
            '2) The sender has permission to forward commands.  Permission here
            '    refers to both relaying And write access, as referenced in command
            '    id 0x4.
            '3) The target Is on the same database as the sending user

            'PROTOCOL ERRORS
            '2 = bad state, client must be visible  [done]
            '3 = no target ID                       [done]
            '4 = no sender string                   [done]
            '5 = no command string                  [done]
            '6 = bad target ID                      [done]
            '7 = empty sender                       [done]
            '8 = empty command                      [done]
            '9 = malformed sender name              [    ] what?
            '   PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_STATE, packet.Length, (packet.Length - packet.Offset))

            If (packet.Length < 8) Then
                'missing target
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.NO_TARGET_ID, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If (packet.Length < 9) Then
                'missing sender
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.NO_SENDER_STRING, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If (packet.Length < 10) Or (packet.Length > 424) Then 'Bad packet (min: 10, max: 424)
                Call Client.Delete()
                Return
            End If
            If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                '2 = bad state, client tried to issue command while invisible 
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim target_bot_id As UInt32 = packet.GetInt32()
            'does this target exist on the server right now?
            If Not CLIENTs.ContainsKey(target_bot_id) Then
                'target does not exist
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.BAD_TARGET_ID, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim sender() As Byte = packet.GetByteString()
            If sender.Length = 1 Then
                'empty sender
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.EMPTY_SENDER, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            'do we have a command message
            If packet.Length = (8 + sender.Length) Then 'bytestring includes the null
                'missing command
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.NO_COMMAND_STRING, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Dim command() As Byte = packet.GetByteString()
            If command.Length = 1 Then
                'empty command
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_COMMAND, PROTOCOL_VIOLATION_COMMAND_8.EMPTY_COMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            SMSG_PACKET_COMMANDOVERBOTNET(Client.AccountUniqueID, PACKET_COMMAND_COMMANDS.DIRECTED_TO_SPECIFIC_CLIENT, sender, command, CLIENTs(target_bot_id))
        End Sub
        Public Sub CMSG_PACKET_MESSAGE(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Debug.Print("CMSG_PACKET_MESSAGE" & vbNewLine)
            '(send to server) id 0x04 command to bots on same database
            '
            'Contents:
            '	(STRING32) sending user
            '	(STRING:384) command
            'Response:   all CLIENTs on the same database as the sending client receive
            '    packet id 0x04.  If the sending client does Not have permission
            '   to relay commands to other clients, only the sending client
            '   receives command id 0x04 from the server.  The sending client
            '   must have write access To the database, Or have restricted
            '   access And the command match a pre-defined list of acceptable
            '   restricted user commands.  If the client does Not meet the access
            '   requirement, no response Is generated to any user (including the sender
            '   itself).
            '
            '
            'SEE PROTOCOL ERRORS FROM 0x07 [done] including the what?

            If (packet.Length < 5) Then
                'missing sender + command
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_MESSAGE, PROTOCOL_VIOLATION_COMMAND_7.MISSING_SENDER_SRTING, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If (packet.Length < 6) Or (packet.Length > 420) Then 'Bad packet (min: 6, max: 420)
                Call Client.Delete()
                Return
            End If
            If (Not ((Client.DatabaseFlags And FLAGS.B) = FLAGS.B)) And (Not ((Client.DatabaseFlags And FLAGS.C) = FLAGS.C)) Then
                'client must have flag "B" or "A"
                'Call Client.Delete()
                Return
            End If
            If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                '2 = bad state, client tried to issue command while invisible 
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_MESSAGE, PROTOCOL_VIOLATION_COMMAND_7.BAD_STATE, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            'Dont need to encode these strip from the packet and fire off
            Dim SenderName() As Byte = packet.GetByteString()
            If (SenderName.Length = 1) Then
                'empty sender
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BROADCASTMESSAGE, PROTOCOL_VIOLATION_COMMAND_7.EMPTY_SENDER, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            If (packet.Length = (SenderName.Length + 4)) Then
                'missing command
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BROADCASTMESSAGE, PROTOCOL_VIOLATION_COMMAND_7.MISSING_COMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            Dim command() As Byte = packet.GetByteString()
            If (command.Length = 1) Then
                'empty command
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BROADCASTMESSAGE, PROTOCOL_VIOLATION_COMMAND_7.EMPTY_COMMAND, packet.Length, (packet.Length - packet.Offset))
                Return
            End If

            'The message goes to every client on this database.
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If ((cliTemp.Value.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                    If cliTemp.Value.DatabaseAccountID = Client.DatabaseAccountID Then 'Must be on the same database
                        SMSG_PACKET_COMMANDOVERBOTNET(Client.AccountUniqueID, PACKET_COMMAND_COMMANDS.SEND_TO_DATABASE, SenderName, command, cliTemp.Value)
                    End If
                End If
            Next

            '            Dim sender As String = packet.GetString
            '            Dim command As String = packet.GetString
            '            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
            '               If cliTemp.Value.DatabaseAccountID = Client.DatabaseAccountID Then
            '                   If ((cliTemp.Value.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
            '                       'do nothing do not send to the user logging in
            '                       '(send to client) id 0x04: command over botnet
            '                       'Contents:
            '                       '	(4.1) (DWORD) sending client's ID
            '                       '	(4.1) (DWORD) distribution status
            '                       '		0 = broadcast to all users
            '                       '		1 = sent to database
            '                       '		2 = directed to this client specifically
            '                       '	(STRING) sender
            '                       '	(STRING) command
            '                       'Response: None.
            '                       Dim sendcommand As New PacketClass(OPCODES.PACKET_MESSAGE)
            '                       If cliTemp.Value.COMMUNICATION_VERSION = SERVER_VERSION.REVISION_1 Then
            '                           sendcommand.AddInt32(Client.AccountUniqueID)                    '4.1
            '                           sendcommand.AddInt32(PACKET_COMMAND_COMMANDS.SEND_TO_DATABASE)  '4.1
            '                       End If
            '                       sendcommand.AddString(Client.Account) 'There was a reason sender string is in the packet... lol
            '                       sendcommand.AddString(command)
            '                       cliTemp.Value.Send(sendcommand)
            '                   Else
            '                       'Cant send to this user they're not logged in
            '                   End If
            '               End If
            '            Next
        End Sub
        Private Sub SMSG_PACKET_COMMANDOVERBOTNET(ByVal sender_id As UInt32, ByVal distro_status As PACKET_COMMAND_COMMANDS, ByVal sender() As Byte, ByVal command() As Byte, ByRef Client As ClientClass)
            Dim response As New PacketClass(OPCODES.PACKET_MESSAGE)
            If Client.COMMUNICATION_VERSION = SERVER_VERSION.REVISION_1 Then
                response.AddInt32(sender_id)        '4.1
                response.AddInt32(distro_status)    '4.1
            End If
            response.AddByteString(sender, sender.Length)
            response.AddByteString(command, command.Length)
            Client.Send(response)
        End Sub
#End Region


        Public Sub SMSG_CHAT_DROP(ByRef Client As ClientClass)
            'Send back their current option values.
            Dim response As New PacketClass(OPCODES.PACKET_CHATDROPOPTIONS)
            response.AddInt8(0) 'Botnet only had command 0
            response.AddInt8(Client.ChatDropOptions.BroadCastedChat)
            response.AddInt8(Client.ChatDropOptions.DatabaseChat)
            response.AddInt8(Client.ChatDropOptions.WhisperChat)
            response.AddInt8(Client.ChatDropOptions.OtherDatabaseChat)
            Client.Send(response)
        End Sub
        Public Sub CMSG_CHAT_DROP(ByRef packet As PacketClass, ByRef Client As ClientClass)
            If packet.Length < 4 Or packet.Length > 9 Then
                'bad packet kill user
                Call Client.Delete()
                Return
            End If
            If packet.Length < 9 Then 'User requesting their info
                SMSG_CHAT_DROP(Client)
                Return
            End If
            'Set their chosen chat drop options
            Dim bIntCommand As UInteger = packet.GetInt8()
            Select Case bIntCommand
                Case 0 '(0 is the only command botnet has for this feature)
                    Client.ChatDropOptions.BroadCastedChat = packet.GetInt8()
                    If (Client.ChatDropOptions.BroadCastedChat < CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL) Then
                        Client.ChatDropOptions.BroadCastedChat = CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL
                    End If
                    If Client.ChatDropOptions.BroadCastedChat > CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL Then
                        Client.ChatDropOptions.BroadCastedChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL
                    End If
                    Client.ChatDropOptions.DatabaseChat = packet.GetInt8()
                    If (Client.ChatDropOptions.DatabaseChat < CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL) Then
                        Client.ChatDropOptions.DatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL
                    End If
                    If Client.ChatDropOptions.DatabaseChat > CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL Then
                        Client.ChatDropOptions.DatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL
                    End If
                    Client.ChatDropOptions.WhisperChat = packet.GetInt8()
                    If (Client.ChatDropOptions.WhisperChat < CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL) Then
                        Client.ChatDropOptions.WhisperChat = CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL
                    End If
                    If Client.ChatDropOptions.WhisperChat > CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL Then
                        Client.ChatDropOptions.WhisperChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL
                    End If
                    Client.ChatDropOptions.OtherDatabaseChat = packet.GetInt8()
                    If (Client.ChatDropOptions.OtherDatabaseChat < CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL) Then
                        Client.ChatDropOptions.OtherDatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL
                    End If
                    If Client.ChatDropOptions.OtherDatabaseChat > CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL Then
                        Client.ChatDropOptions.OtherDatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL
                    End If
                    Exit Select
            End Select
            SMSG_CHAT_DROP(Client)
        End Sub

        Public Sub CMSG_PACKET_BOTNETCHAT(ByRef packet As PacketClass, ByRef Client As ClientClass)
            Debug.Print("CMSG_PACKET_BOTNETCHAT" & vbNewLine)
            '	(DWORD) command
            '		0	: message to all bots
            '		->1	: message to bots on the same database
            '		2	: message to bot specified by id.
            '	(DWORD) action	: 0x00=talk, 0x01=emote, any other is dropped
            '	(DWORD) id	: for command 0x02, id of bot to send to, otherwise ignored.
            '	(STRING:496) message: blank messages are dropped
            'Bad packet check (min: 17, max: 512)
            If packet.Length < 17 Or packet.Length > 512 Then
                'bad packet kill user
                Call Client.Delete()
                Return
            End If

            'Check client state.
            If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                '2 = bad state, client must be visible
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 2, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            'check packet size (min: 17 max: 512)
            If packet.Length < 17 Or packet.Length > 512 Then '
                If packet.Length = 16 Then
                    'missing text
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 6, packet.Length, (packet.Length - packet.Offset))
                    Return
                End If
                If packet.Length > 12 And packet.Length < 16 Then
                    'missing id
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 5, packet.Length, (packet.Length - packet.Offset))
                    Return
                End If
                If packet.Length > 8 And packet.Length < 12 Then
                    'missing action
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 4, packet.Length, (packet.Length - packet.Offset))
                    Return
                End If
                If packet.Length > 4 And packet.Length < 8 Then
                    'missing command
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 3, packet.Length, (packet.Length - packet.Offset))
                    Return
                End If
                Return
            End If
            '    command 11
            '2 = bad state, client must be visible                  [done]
            '3 = missing distribution level                         [done]
            '4 = missing emote                                      [done]
            '5 = missing target id                                  [done]
            '6 = missing text                                       [done]
            '7 = bad distribution level                             [done]
            '8 = bad target (only if attempting to whisper a user)  [done]
            '                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 7, packet.Length, (packet.Length - packet.Offset))

            Dim command As PACKET_COMMAND_COMMANDS = packet.GetInt32
            If (command < 0) Or (command > 2) Then
                '7 = bad distribution level
                PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 7, packet.Length, (packet.Length - packet.Offset))
                Return
            End If
            Dim action As UInt32 = packet.GetInt32
            If (action < 0) Or (action > 1) Then
                'bad action
                Return
            End If
            Dim userid As UInt32 = packet.GetInt32
            If command = PACKET_COMMAND_COMMANDS.DIRECTED_TO_SPECIFIC_CLIENT Then
                If Not CLIENTs.ContainsKey(userid) Then
                    '8 = bad target (only if attempting to whisper a user)
                    PROTOCOL_VIOLATION(Client, OPCODES.PACKET_BOTNETCHAT, 8, packet.Length, (packet.Length - packet.Offset))
                    Return 'The user in question does not exist, stop wasting time here.
                End If
            End If

            Dim message() As Byte = packet.GetByteString()
            If message.Length = 1 Then 'we are not sending out blank messages (length 1 = null message)
                Return
            End If

            Select Case command
                Case PACKET_COMMAND_COMMANDS.BROADCAST_TO_ALL_USERS
                    CHAT_MESSAGE_TO_ALL_USERS(Client, command, action, message)
                Case PACKET_COMMAND_COMMANDS.SEND_TO_DATABASE
                    CHAT_MESSAGE_TO_ALL_USERS(Client, command, action, message)
                Case PACKET_COMMAND_COMMANDS.DIRECTED_TO_SPECIFIC_CLIENT
                    CHAT_MESSAGE_TO_USER(Client, command, action, userid, message)
            End Select
        End Sub
        Private Sub CHAT_MESSAGE_TO_ALL_USERS(ByRef Client As ClientClass, ByVal command As PACKET_COMMAND_COMMANDS, ByVal action As PACKET_COMMAND_ACTIONS, ByVal message() As Byte)
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If Not (cliTemp.Value.Account = Client.Account) Then '
                    'Enable ChatDrop Fire this message through CHAT_MESSAGE_TO_USER pump since it has all the checking finnished.
                    CHAT_MESSAGE_TO_USER(Client, command, action, cliTemp.Value.AccountUniqueID, message)
                End If
            Next
        End Sub
        'THERE CAN BE ONLY 1 (dont need this function it does the database checks in the actual send message now)
        'Private Sub CHAT_MESSAGE_ALL_ON_DATABASE(ByRef Client As ClientClass, ByVal command As PACKET_COMMAND_COMMANDS, ByVal action As PACKET_COMMAND_ACTIONS, ByVal message() As Byte)
        '    For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
        '        If Not (cliTemp.Value.Account = Client.Account) Then
        '            'Enable ChatDrop Fire this message through CHAT_MESSAGE_TO_USER pump since it has all the checking finnished.
        '            CHAT_MESSAGE_TO_USER(Client, command, action, cliTemp.Value.AccountUniqueID, message)
        '        End If
        '    Next
        'End Sub
        Private Sub CHAT_MESSAGE_TO_USER(ByRef Client As ClientClass, ByVal command As PACKET_COMMAND_COMMANDS, ByVal action As PACKET_COMMAND_ACTIONS, ByVal userid As UInt32, ByVal message() As Byte)
            'Is the sender logged in yet.
            If Not ((Client.STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                Return 'This right here should be a PROTOCOL_VIOLATION
            End If
            'Does the user in question exist
            If Not CLIENTs.ContainsKey(userid) Then
                Return 'The user in question does not exist, stop wasting time here.
            End If
            'is the user in question logged in
            If Not ((CLIENTs(userid).STATE And STATE_FLAGS.ACCOUNT_LOGGED_IN) = STATE_FLAGS.ACCOUNT_LOGGED_IN) Then
                Return 'this user is not online yet
            End If



#Region "---- CHAT DROP OPTION CHECKS ----"
            'BROADCAST
            Select Case command
#Region "Broadcasted to all"
                Case PACKET_COMMAND_COMMANDS.BROADCAST_TO_ALL_USERS
                    'are we blocking database messages
                    If CLIENTs(userid).ChatDropOptions.BroadCastedChat > CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL Then
                        If CLIENTs(userid).ChatDropOptions.BroadCastedChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL Then
                            Return
                        End If
                        'are we blocking <no account>s
                        If CLIENTs(userid).ChatDropOptions.BroadCastedChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_FROM_NOACCOUNT Then
                            If Client.Account = "" Or Client.Account.ToLower = "<no account>" Then 'I donot believe i added the bracketed no account name to the server but anyways
                                Return
                            End If
                        End If
                    End If
                    Exit Select
#End Region
#Region "To User (whispers)"
                Case PACKET_COMMAND_COMMANDS.DIRECTED_TO_SPECIFIC_CLIENT
                    If CLIENTs(userid).ChatDropOptions.WhisperChat > CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL Then
                        If CLIENTs(userid).ChatDropOptions.WhisperChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL Then
                            Return
                        End If
                        'are we blocking <no account>s
                        If CLIENTs(userid).ChatDropOptions.WhisperChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_FROM_NOACCOUNT Then
                            If Client.Account = "" Or Client.Account.ToLower = "<no account>" Then 'I donot believe i added the bracketed no account name to the server but anyways
                                Return
                            End If
                        End If
                    End If
                    Exit Select
#End Region
#Region "To Database"
                Case PACKET_COMMAND_COMMANDS.SEND_TO_DATABASE
                    'Is this user the same database
                    If Not (Client.DatabaseAccountID.ToLower = CLIENTs(userid).DatabaseAccountID.ToLower) Then
                        Return
                    End If
                    'chatdrop.
                    If CLIENTs(userid).ChatDropOptions.DatabaseChat > CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL Then
                        If CLIENTs(userid).ChatDropOptions.DatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL Then
                            Return
                        End If
                        'are we blocking <no account>s
                        If CLIENTs(userid).ChatDropOptions.DatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_FROM_NOACCOUNT Then
                            If Client.Account = "" Or Client.Account.ToLower = "<no account>" Then 'I donot believe i added the bracketed no account name to the server but anyways
                                Return
                            End If
                        End If
                    End If
                    Exit Select
#End Region
                Case Else 'bad command
                    Return
            End Select
            'is this message from somone on another database.
            If CLIENTs(userid).ChatDropOptions.OtherDatabaseChat > CHAT_DROP_OPTIONS_SETTINGS.ALLOW_ALL Then
                If CLIENTs(userid).ChatDropOptions.OtherDatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_ALL Then
                    If Not (Client.DatabaseAccountID.ToLower = CLIENTs(userid).DatabaseAccountID.ToLower) Then 'I donot believe i added the bracketed no account name to the server but anyways
                        Return
                    End If
                End If
                If CLIENTs(userid).ChatDropOptions.OtherDatabaseChat = CHAT_DROP_OPTIONS_SETTINGS.REFUSE_FROM_NOACCOUNT Then
                    If Client.Account = "" Or Client.Account.ToLower = "<no account>" Then 'I donot believe i added the bracketed no account name to the server but anyways
                        Return
                    End If
                End If
            End If
#End Region

            '
            'ALL CHECKS ARE IN THE ABOVE CODE
            'IF WE GOT HERE FIRE OFF THE MESSAGE
            '
            Dim sendmessage As New PacketClass(OPCODES.PACKET_BOTNETCHAT)
            sendmessage.AddInt32(command)
            sendmessage.AddInt32(action)
            sendmessage.AddInt32(Client.AccountUniqueID)
            sendmessage.AddByteString(message, message.Length)
            CLIENTs(userid).Send(sendmessage)
        End Sub
#End Region

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

        Private Function GetUserIndex(ByVal AccountName As String, Optional NotSelf As UInt32 = 0) As UInt32
            For Each cliTemp As KeyValuePair(Of UInteger, ClientClass) In CLIENTs
                If cliTemp.Value.Account.ToLower = AccountName.ToLower Then
                    If NotSelf > 0 Then
                        'if > 0 we passed it our index test now.
                        If Not (cliTemp.Value.Index = NotSelf) Then
                            'if the index is not ours we found it
                            Return cliTemp.Value.Index
                        End If
                    Else
                            Return cliTemp.Value.Index
                    End If
                End If
            Next
            Return 0 '0 = no account found using this name
        End Function

        Public Sub New()
            IntializePacketHandlers()
            KeepAliveTimer = New Timers.Timer
            KeepAliveTimer.Interval = 110000
            KeepAliveTimer.Enabled = False
        End Sub

        Private Sub KeepAliveTimer_Elapsed(ByVal sender As Object, ByVal e As System.Timers.ElapsedEventArgs) Handles KeepAliveTimer.Elapsed
            Dim klPacket As New PacketClass(OPCODES.PACKET_IDLE)
            Send(klPacket)
        End Sub
    End Class


End Module
