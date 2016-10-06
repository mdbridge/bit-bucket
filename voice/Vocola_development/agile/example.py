###
### Tokenizer for Vocola using PLX
### 

import lex as lex


literals = '()[]|,;=' + ':'    # allow : for now <<<>>>
t_ASSIGN = ':='

tokens = (
   'ASSIGN',
   'DOUBLE_QUOTE',
   'SINGLE_QUOTE',
   'BAD_DOUBLE_QUOTE',
   'BAD_SINGLE_QUOTE',
   'RAW',
)


#
# Whitespace.
# 

t_ignore  = ' \t\r'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_ignore_COMMENT(t):
    r'\#[^\n]*\n'
    t.lexer.lineno += 1


#
# Quoted strings.
#
#   Within a string, doubling the delimiter codes for that delimiter.
#   Example: "one "" quote" has the same value as 'one " quote'.
#
#   Strings may not span multiple lines.
#

def t_DOUBLE_QUOTE(t):
    r'"([^"\n]|"")*["\n]'
    if t.value[-1] == '\n':
        t.type = 'BAD_DOUBLE_QUOTE'
        t.lexer.lineno += 1
    t.value = t.value[1:-1].replace('""', '"')
    return t

def t_SINGLE_QUOTE(t):
    r"'([^'\n]|'')*['\n]"
    if t.value[-1] == '\n':
        t.type = 'BAD_SINGLE_QUOTE'
        t.lexer.lineno += 1
    t.value = t.value[1:-1].replace("''", "'")
    return t


#
# Raw words.
# 
#   Can contain any character except for:
# 
#     \s # '" () [] | , ; =
#
#   and cannot end with :
#

def t_RAW(t):
    r'[^\s\#"' "'" r'()\[\]|,;=]*[^\s\#"' "'" r'()\[\]|,;=:]'
    return t


# Error handling rule  <<<>>>
def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)



lexer = lex.lex()



##
## When run as a stand-alone program:
## 

if __name__ == "__main__":
    import sys

    data = sys.stdin.read() + '\n'
    lexer.input(data)

    while True:
        tok = lexer.token()
        if not tok: break      # No more input
        print tok
