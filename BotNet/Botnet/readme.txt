What is fully supported at the moment, along with their error codes, + some new ones that can be ignored if you like.

----------------------------------------------------------------------------------
#define MSG_HUB_LOGIN 0x01

enum PROTOCOL_VIOLATION_HUB_LOGIN {
    CLIENT_ATTEMPTED_AUTHING_A_SECOND_TIME = 2
    BAD_STRLEN_HUBID = 3
    BAD_STRLEN_HUB_PASSWORD = 4
    STR_BLANK_HUBID = 5
    STR_BLANK_HUB_PASSWORD = 6
}

enum HUB_LOGIN_RESPONSE {
    Accepted = 0x01
    Failed = 0x00
}

C>S 0x01: feild
String HUB_ID
String HUB_PASSWORD

S>C 0x01: feild
UInt32 response
UInt32 client_ipv4_address

----------------------------------------------------------------------------------
#define MSG_STATUS_UPDATE 0x02

enum PROTOCOL_VIOLATION_STATUS_UPDATE {
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
}

enum STATUS_UPDATE_RESPONSE {
    Accepted = 1
    Failed = 0
}

C->S 0x02: feild
String BNet_Name
String BNet_Channel
UInt32 BNet_IPv4_server
String DatabaseInfo_space_DatabasePassword
UInt32 IsCycleing_BOOL

S->C 0x02: feild
UInt32 responce_BOOL
UInt32 Account_Flag (not seen in initial login process)

----------------------------------------------------------------------------------

Adding bit by bit as I go over the current code.
