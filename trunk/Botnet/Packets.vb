﻿Imports System.IO

Public Module Packets
#Region "Handlers And OP_CODES"
    Public Enum OPCODES
        PACKET_IDLE = 0                 '(0x00) 'done on timer
        PACKET_LOGON = 1                '(0x01) 'done
        PACKET_STATSUPDATE = 2          '(0x02) 'done
        PACKET_DATABASE = 3             '(0x03)
        PACKET_MESSAGE = 4              '(0x04) 'done
        PACKET_CYCLE = 5                '(0x05)
        PACKET_USERINFO = 6             '(0x06) 'done
        PACKET_BROADCASTMESSAGE = 7     '(0x07)
        PACKET_COMMAND = 8              '(0x08) 'done
        PACKET_PROTOCOL_VIOLATION = 8   '(0x08) 'done
        PACKET_CHANGEDBPASSWORD = 9     '(0x09) '
        PACKET_BOTNETVERSION = 10       '(0x0a) '
        PACKET_BOTNETCHAT = 11          '(0x0b) 'done
        PACKET_ACCOUNT = 13             '(0x0d)
        PACKET_CHATDROPOPTIONS = 16     '(0x10)
    End Enum
    Public Enum sOPCODES
        PACKET_USERLOGGINGOFF = 7       '(0x07) 'done on disconnects
    End Enum
    Public PacketHandlers As New Dictionary(Of OPCODES, HandlePacket)
    Delegate Sub HandlePacket(ByRef Packet As PacketClass, ByRef Client As ClientClass)
#End Region
    Public Const BOTNET_PROTO_VERSION As Integer = 4

    Public Class PacketClass
        Implements IDisposable

        Public Data() As Byte
        Public Offset As Integer = 4

        Public ReadOnly Property Length() As Integer
            Get
                Return (Data(2) + (Data(3) * 256))
            End Get
        End Property
        Public ReadOnly Property OpCode() As OPCODES
            Get
                Return (Data(1)) ' + (Data(3) * 256))
            End Get
        End Property
        Public ReadOnly Property ProtocalVersion() As OPCODES
            Get
                Return (Data(0)) ' + (Data(3) * 256))
            End Get
        End Property

        Public Sub New(ByVal opcode As OPCODES)
            ReDim Preserve Data(3)
            Data(0) = BOTNET_PROTO_VERSION
            Data(1) = CType(opcode, Byte)
            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256
        End Sub
        Public Sub New(ByRef rawdata() As Byte)
            Data = rawdata
            rawdata.CopyTo(Data, 0)
        End Sub


        Public Sub AddBitArray(ByVal buffer As BitArray, ByVal Len As Integer)
            ReDim Preserve Data(Data.Length - 1 + Len)
            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256

            Dim bufferarray(CType((buffer.Length + 8) / 8, Byte)) As Byte

            buffer.CopyTo(bufferarray, 0)
            Array.Copy(bufferarray, 0, Data, Data.Length - Len, Len)
        End Sub
        Public Sub AddInt8(ByVal buffer As Byte)
            ReDim Preserve Data(Data.Length)
            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256
            Data(Data.Length - 1) = buffer
        End Sub
        Public Sub AddInt16(ByVal buffer As Short)
            ReDim Preserve Data(Data.Length + 1)
            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256

            'Data(Data.Length - 2) = buffer Mod 256
            'Data(Data.Length - 1) = buffer \ 256

            Data(Data.Length - 2) = CType((buffer And 255), Byte)
            Data(Data.Length - 1) = CType(((buffer >> 8) And 255), Byte)
        End Sub
        Public Sub AddInt32(ByVal buffer As Integer)
            ReDim Preserve Data(Data.Length + 3)
            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256

            'Data(Data.Length - 4) = buffer Mod 256
            'Data(Data.Length - 3) = (buffer \ 256) Mod 256
            'Data(Data.Length - 2) = ((buffer \ 256) \ 256) Mod 256
            'Data(Data.Length - 1) = ((buffer \ 256) \ 256) \ 256

            Data(Data.Length - 4) = CType((buffer And 255), Byte)
            Data(Data.Length - 3) = CType(((buffer >> 8) And 255), Byte)
            Data(Data.Length - 2) = CType(((buffer >> 16) And 255), Byte)
            Data(Data.Length - 1) = CType(((buffer >> 24) And 255), Byte)
        End Sub
        Public Sub AddInt64(ByVal buffer As Long)
            ReDim Preserve Data(Data.Length + 7)
            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256

            Data(Data.Length - 8) = CType((buffer And 255), Byte)
            Data(Data.Length - 7) = CType(((buffer >> 8) And 255), Byte)
            Data(Data.Length - 6) = CType(((buffer >> 16) And 255), Byte)
            Data(Data.Length - 5) = CType(((buffer >> 24) And 255), Byte)
            Data(Data.Length - 4) = CType(((buffer >> 32) And 255), Byte)
            Data(Data.Length - 3) = CType(((buffer >> 40) And 255), Byte)
            Data(Data.Length - 2) = CType(((buffer >> 48) And 255), Byte)
            Data(Data.Length - 1) = CType(((buffer >> 56) And 255), Byte)
        End Sub
        Public Sub AddString(ByVal buffer As String)
            Dim Bytes As Byte() = System.Text.Encoding.UTF8.GetBytes(buffer.ToCharArray)

            ReDim Preserve Data(Data.Length + Bytes.Length)
            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256

            Dim i As Integer
            For i = 0 To Bytes.Length - 1
                Data(Data.Length - 1 - Bytes.Length + i) = Bytes(i)
            Next i

            Data(Data.Length - 1) = 0
        End Sub
        Public Sub AddDouble(ByVal buffer2 As Double)
            Dim buffer1 As Byte() = BitConverter.GetBytes(buffer2)
            ReDim Preserve Data(Data.Length + buffer1.Length - 1)
            Buffer.BlockCopy(buffer1, 0, Data, Data.Length - buffer1.Length, buffer1.Length)

            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256
        End Sub
        Public Sub AddSingle(ByVal buffer2 As Single)
            Dim buffer1 As Byte() = BitConverter.GetBytes(buffer2)
            ReDim Preserve Data(Data.Length + buffer1.Length - 1)
            Buffer.BlockCopy(buffer1, 0, Data, Data.Length - buffer1.Length, buffer1.Length)

            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256
        End Sub
        Public Sub AddByteArray(ByVal buffer() As Byte)
            Dim tmp As Integer = Data.Length
            ReDim Preserve Data(Data.Length + buffer.Length - 1)
            Array.Copy(buffer, 0, Data, tmp, buffer.Length)

            Data(2) = (Data.Length) Mod 256
            Data(3) = (Data.Length) \ 256
        End Sub


        Public Function GetInt8() As Byte
            Offset = Offset + 1
            Return Data(Offset - 1)
        End Function
        Public Function GetInt8(ByVal Offset As Integer) As Byte
            Offset = Offset + 1
            Return Data(Offset - 1)
        End Function
        Public Function GetInt16() As Short
            Dim num1 As Short = BitConverter.ToInt16(Data, Offset)
            Offset = (Offset + 2)
            Return num1
        End Function
        Public Function GetInt16(ByVal Offset As Integer) As Short
            Dim num1 As Short = BitConverter.ToInt16(Data, Offset)
            Offset = (Offset + 2)
            Return num1
        End Function
        Public Function GetInt32() As Integer
            Dim num1 As Integer = BitConverter.ToInt32(Data, Offset)
            Offset = (Offset + 4)
            Return num1
        End Function
        Public Function GetInt32(ByVal Offset As Integer) As Integer
            Dim num1 As Integer = BitConverter.ToInt32(Data, Offset)
            Offset = (Offset + 4)
            Return num1
        End Function
        Public Function GetInt64() As Long
            Dim num1 As Long = BitConverter.ToInt64(Data, Offset)
            Offset = (Offset + 8)
            Return num1
        End Function
        Public Function GetInt64(ByVal Offset As Integer) As Long
            Dim num1 As Long = BitConverter.ToInt64(Data, Offset)
            Offset = (Offset + 8)
            Return num1
        End Function
        Public Function GetFloat() As Single
            Dim single1 As Single = BitConverter.ToSingle(Data, Offset)
            Offset = (Offset + 4)
            Return single1
        End Function
        Public Function GetFloat(ByVal Offset As Integer) As Single
            Dim single1 As Single = BitConverter.ToSingle(Data, Offset)
            Offset = (Offset + 4)
            Return single1
        End Function
        Public Function GetDouble() As Double
            Dim num1 As Double = BitConverter.ToDouble(Data, Offset)
            Offset = (Offset + 8)
            Return num1
        End Function
        Public Function GetDouble(ByVal Offset As Integer) As Double
            Dim num1 As Double = BitConverter.ToDouble(Data, Offset)
            Offset = (Offset + 8)
            Return num1
        End Function
        Public Function GetString() As String
            Dim start As Integer = Offset
            Dim i As Integer = 0

            While Data(start + i) <> 0
                i = i + 1
                Offset = Offset + 1
            End While
            Offset = Offset + 1

            Return System.Text.Encoding.UTF8.GetString(Data, start, i)
        End Function
        Public Function GetString(ByVal Offset As Integer) As String
            Dim i As Integer = Offset
            Dim tmpString As String = ""
            While Data(i) <> 0
                tmpString = tmpString + Chr(Data(i))
                i = i + 1
                Offset = Offset + 1
            End While
            Offset = Offset + 1
            Return tmpString
        End Function
        Public Sub Dispose() Implements System.IDisposable.Dispose
            '
        End Sub
    End Class
    Public Class UpdatePacketClass
        Inherits PacketClass

        Public Property UpdatesCount() As Integer
            Get
                Return BitConverter.ToInt32(Data, 4)
            End Get
            Set(ByVal Value As Integer)
                '    Data(4) = CType((Value And 255), Byte)
                '    Data(5) = CType(((Value >> 8) And 255), Byte)
                '    Data(6) = CType(((Value >> 16) And 255), Byte)
                '    Data(7) = CType(((Value >> 24) And 255), Byte)
            End Set
        End Property
        Public Sub New()
            MyBase.New(OPCODES.PACKET_IDLE)

            'Me.AddInt32(0)
            'Me.AddInt8(0)
        End Sub

        Public Sub Compress()
        End Sub
    End Class

End Module
