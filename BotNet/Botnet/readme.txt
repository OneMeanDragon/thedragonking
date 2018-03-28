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

enum HUB_LOGIN_R {
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
