Read the Official Doc on whats listed here.
----------------------------------------------------------------------------------
[Fully Supported]
0x00	PACKET_IDLE
0x01	PACKET_LOGON
0x06	PACKET_USERINFO
0x0B	PACKET_BOTNETCHAT
0x0D	PACKET_ACCOUNT
0x10	PACKET_CHATDROPOPTIONS
	
[Supported at bare minimum]
0x02	PACKET_STATSUPDATE <cycleing needs more info>
0x0A	PACKET_BOTNETVERSION <(DWORD) client capabilities>
0x04	PACKET_MESSAGE <malformed sender not imp>
0x07	PACKET_BROADCASTMESSAGE	<malformed sender not imp>
0x08	PACKET_COMMAND <malformed sender not imp>

[Working But not checked for violations]

[Not implemented as of yet]
0x03	PACKET_DATABASE
0x05	PACKET_CYCLE
0x09	PACKET_CHANGEDBPASSWORD
----------------------------------------------------------------------------------

-l)ragon
