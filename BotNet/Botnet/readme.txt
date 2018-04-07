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
UInt32 HUB_LOGIN_RESPONSE (BOOL)
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
    BAD_STATE = 15
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
UInt32 STATUS_UPDATE_RESPONSE (BOOL)
UInt32 Account_Flag (only seen if your account flag changes, if this DWORD is not present your prev flag hasent changed values)

----------------------------------------------------------------------------------
#define MSG_ACCOUNT 0xD

enum PROTOCOL_VIOLATION_MSG_ACCOUNT {
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
}

enum MSG_ACCOUNT_RESPONSE {
    Accepted = 1
    Failed = 0
}

C->S 0xD: feild
UInt32 Command
String accountname
if(command == 0) { //Login
    string accountpassword
}
if (command == 1) { //Change password
    string oldpassword
    string newpassword
}
if (command == 2) { //Account create
    string accountpassword
}

S-> 0xD: feild
UInt32 Command
UInt32 MSG_ACCOUNT_RESPONSE (BOOL)

----------------------------------------------------------------------------------


Adding bit by bit as I go over the current code.
