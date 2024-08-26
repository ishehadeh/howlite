%%

0x[0-9a-fA-F](_?[0-9a-fA-F])*  "UINT16"
[0-9](_?[0-9])*                "UINT10"
0o[0-7](_?[0-7])*              "UINT8"
0b[01](_?[01])*                "UINT2"

-                              "MINUS"
\+                             "PLUS"

\r\n          "CRLF"
\n            "LF"
[ ]+          "SPACE"
\t+           "TAB"
//[\x31-\xfe \t]*                   "COMMENT-LINE"
/\*([\x31-\xfe \t\n]|(\r\n))*\*/    "COMMENT-MULTILINE"


