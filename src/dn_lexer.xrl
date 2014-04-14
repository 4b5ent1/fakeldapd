Definitions.

NULL           = \x00
SPACE          = \x20
DQUOTE         = \x22
SHARP          = \x23
DOLLAR         = \x24
SQUOTE         = \x27
LPAREN         = \x28
RPAREN         = \x29
PLUS           = \x2B
COMMA          = \x2C
HYPHEN         = \x2D
DOT            = \x2E
SEMI           = \x3B
LANGLE         = \x3C
EQUALS         = \x3D
RANGLE         = \x3E
ESC            = \x5C
USCORE         = \x5F
LCURLY         = \x7B
RCURLY         = \x7D

SP             = {SPACE}+
WSP            = {SPACE}*

ALPHA          = [\x41-\x5A]|[\x61-\x7A]
LDIGIT         = [\x31-\x39]
DIGIT          = \x30|{LDIGIT}
HEX            = {DIGIT}|[\x41-\x46]|[\x61-\x66]
LEADKEYCHAR    = {ALPHA}
KEYCHAR        = {ALPHA}|{DIGIT}|{HYPHEN}

NUMBER         = {DIGIT}|({LDIGIT}(({DIGIT})+))
KEYSTRING      = {LEADKEYCHAR}(({KEYCHAR})*)

UTF0           = [\x80-\xBF]
UTF1           = [\x00-\x7F]
UTF2           = [\xC2-\xDF]{UTF0}
UTF3           = (\xE0[\xA0-\xBF]{UTF0})|([\xE1-\xEC]{UTF0}{UTF0})|(\xED[\x80-\x9F]{UTF0})|([\xEE-\xEF]{UTF0}{UTF0})
UTF4           = (\xF0[\x90-\xBF]{UTF0}{UTF0})|([\xF1-\xF3]{UTF0}{UTF0}{UTF0})|(\xF4[\x80-\x8F]{UTF0}{UTF0})
UTFMB          = ({UTF2})|({UTF3})|({UTF4})
UTF8           = ({UTF1})|({UTFMB})
OCTET          = [\x00-\xFF]

NUMERICOID     = {NUMBER}(({DOT})(({NUMBER})+))
DESCR          = {KEYSTRING}

LUTF1          = [\x01-\x1F]|\x21|[\x24-\x2A]|[\x2D-\x3A]|[\x3F-\x5B]|[\x5D-\x7F]
LEADCHAR       = ({LUTF1})|({UTFMB})

TUTF1          = [\x01-\x1F]|\x21|[\x23-\x2A]|[\x2D-\x3A]|[\x3F-\x5B]|[\x5D-\x7F]
TRAILCHAR      = ({TUTF1})|({UTFMB})

SUTF1          = [\x01-\x21]|[\x23-\x2A]|[\x2D-\x3A]|[\x3F-\x5B]|[\x5D-\x7F]
STRINGCHAR     = ({SUTF1})|({UTFMB})

HEXPAIR        = ({HEX})({HEX})
HEXSTRING      = ({SHARP})(({HEXPAIR})+)

ESCAPED        = {DQUOTE}|{PLUS}|{COMMA}|{SEMI}|{LANGLE}|{RANGLE}
SPECIAL        = {ESCAPED}|{SPACE}|{SHARP}|{EQUALS}
PAIR           = {ESC}(({ESC})|({SPECIAL})|({HEXPAIR}))

STRING         = (({LEADCHAR})|({PAIR}))((({STRINGCHAR})|({PAIR}))*(({TRAILCHAR})|({PAIR})))?

ATTRIBUTETYPE  = ({DESCR})|({NUMERICOID})
ATTRIBUTEVALUE = ({STRING})|({HEXSTRING})

Rules.

{EQUALS}         : {token, {'=',   TokenLine}}.
{PLUS}           : {token, {'+',   TokenLine}}.
{COMMA}          : {token, {',',   TokenLine}}.
{KEYSTRING}      : {token, {type,  TokenLine, TokenChars}}.
{ATTRIBUTEVALUE} : {token, {value, TokenLine, TokenChars}}.

Erlang code.
