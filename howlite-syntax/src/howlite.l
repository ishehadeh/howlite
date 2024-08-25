%%

\r\n          "CRLF"
\n            "LF"
[ ]+          "SPACE"
\t+           "TAB"
//[\x31-\xfe \t]*                   "COMMENT-LINE"
/\*([\x31-\xfe \t\n]|(\r\n))*\*/    "COMMENT-MULTILINE"

