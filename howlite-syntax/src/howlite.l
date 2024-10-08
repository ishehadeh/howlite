%%
unit     "unit"
let      "let"
type     "type"
alias    "alias"
mut      "mut"
func     "func"
if 'if'
else 'else'
while 'while'
extern 'extern'
use 'use'

[_a-zA-Z][_a-zA-Z0-9]*         "IDENT"
0x[0-9a-fA-F](_?[0-9a-fA-F])*  "UINT16"
[0-9](_?[0-9])*                "UINT10"
0o[0-7](_?[0-7])*              "UINT8"
0b[01](_?[01])*                "UINT2"

-                              "-"
\+                             "+"
\*                             "*"
\/                              "/"
\<\<    "<<"
\>\>    ">>"
&     "&"
\|     "|"
\^     "^"
&&       "&&"
\|\|     "||"

\(                             "("
\)                             ")"
\[    "["
\]    "]"

:  ":"
;  ";"
\.\.   ".."
\{    "{"
\}    "}"
\&\[   "&["
\<\{    "<{"
\}\>    "}>"
,  ","
\.  "."
\<=  "<="
\<=  ">="
\<   "<"
\>   ">"
==   "=="
!=   "!="
=                             "="
!  "!"
~  "~"
"((\\["nrt])|(\\x[0-9a-zA-Z]{2})|(\\u[0-9a-zA-Z]{4})|(\\*U[0-9a-zA-Z]{8})|[\pL\pM\pN\pS\pP\p{Zs}])*?"  "STRING"
'((\\['nrt])|(\\x[0-9a-zA-Z]{2})|(\\u[0-9a-zA-Z]{4})|(\\*U[0-9a-zA-Z]{8})|[\pL\pM\pN\pS\pP\p{Zs}])'    "CHAR"

\r\n          "CRLF"
\n            "LF"
[ ]+          "SPACE"
\t+           "TAB"
//[\pL\pM\pN\pS\pP\p{Zs}]*                   "COMMENT-LINE"
/\*[\pL\pM\pN\pS\pP\p{Zs}\p{Zl}]*\*/    "COMMENT-MULTILINE"

