###
### Tokenizer for Vocola 2.7 using PLY
### 
###   By design, never generates an error.
### 

import re

import lex



##
## Token rules for PLY's lex:
## 

literals = '()[]|,;='

t_ASSIGN = ':='

tokens = (
   'ASSIGN',

   'DOUBLE_QUOTE',
   'SINGLE_QUOTE',
   'BAD_DOUBLE_QUOTE',
   'BAD_SINGLE_QUOTE',

   'RAW',
   'NAME',
   'VARIABLE',
   'RANGE',

   'IMPORT',

   'BAD_CONTEXT',
   'UNHANDLED_BAD_CONTEXT',

   'ILLEGAL_CHAR',               # should never occur...
)



#
# partial support for legacy context statements: <<<>>>
# 

def t_BAD_CONTEXT(t):
    r'[^\n#"' "'" r':;]*:[\n \t\r]'
    if t.value[-1] == "\n":
        t.lexer.lineno += 1
    t.value = t.value[0:-2]
    return t

def t_error(t):
    if t.value[0] == ':':
        t.type = 'UNHANDLED_BAD_CONTEXT'
        t.value = t.value[0]
        t.lexer.skip(1)
        return t

    # this should never occur: <<<>>>
    t.type = 'ILLEGAL_CHAR'
    t.value = t.value[0]
    t.lexer.skip(1)
    return t


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
    quotation = t.value
    if quotation[-1] == '\n':
        t.type = 'BAD_DOUBLE_QUOTE'
        t.lexer.lineno += 1
    t.value = (t.value[1:-1].replace('""', '"'), quotation)
    return t

def t_SINGLE_QUOTE(t):
    r"'([^'\n]|'')*['\n]"
    quotation = t.value
    if quotation[-1] == '\n':
        t.type = 'BAD_SINGLE_QUOTE'
        t.lexer.lineno += 1
    t.value = (t.value[1:-1].replace("''", "'"), quotation)
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

    if t.value == "$import":
        t.type = 'IMPORT'
        return t

    m = re.match(r'[a-zA-Z_-][a-zA-Z0-9_-]*$', t.value)
    if m:
        t.type = 'NAME'
        return t

    m = re.match(r'<[a-zA-Z0-9_-]+>$', t.value)
    if m:
        t.type = 'VARIABLE'
        t.value = t.value[1:-1]
        return t

    m = re.match(r'(\d+)\.\.(\d+)$', t.value)
    if m:
        t.type = 'RANGE'
        t.value = (m.group(1), m.group(2))
        return t

    return t



##
## Building the lexer:
## 

def preprocess_input(text):
    text = text.replace("\r\n", "\n")

    # handle legacy include directives:
    text = re.sub(r'(^|\n)include(\s)', r'\1$import\2', text)

    # simplify lexing by ensuring all lines end with a newline:
    return text + "\n"

lexer = lex.lex()



##
## When run as a stand-alone program:
## 

if __name__ == "__main__":
    import sys

    data = sys.stdin.read()
    data = preprocess_input(data)
    lexer.input(data)

    while True:
        tok = lexer.token()
        if not tok:
            break      # No more input
        print tok
