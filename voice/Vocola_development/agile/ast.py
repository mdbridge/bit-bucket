###
### AST routines:
### 

import re


##
## Creating nodes:
## 

def make_node(type, location, *slots):
    return (type,  location) + slots


##
## Accessing nodes:
## 

def node_type(node):
    return node[0]

def loc(node):
    return node[1]

def slot(node, i):
    return node[i+2]

def slots(node):
    return node[2:]


##
## Pretty printing:
## 

formats = {
    # special statements:
    "IMPORT"          : ("$import %s;\n",    ""),
    "CONTEXT"         : ("\n%s:\n",          ""),

    # functions:
    "FUNCTION"        : ("%s(%s) := %s;\n",  ""),
    "FORMAL"          : ("%s",               ", "),

    "CALL"            : ("%s(%s)",           " "),
    "ARGUMENT"        : ("%s",               ", "),

    "REF"             : ("$%s",              " "),
    "STRING"          : ("%s",               " "),

    # commands:
    "COMMAND"         : ("%s = %s;\n",       ""),

    "WORD"            : ("%s",               " "),
    "LIST"            : ("<%s>",             " "),
    "RANGE"           : ("%s..%s",           " "),
    "OPTIONAL"        : ("[%s]",             " "),
    "INLINE-LIST"     : ("(\n    %s\n  )",   " "),

    # lists:
    "LIST-DEFINITION" : ("<%s> := %s;\n",    ""),
    "ALTERNATIVE"     : ("%s",               "\n  | "),
    "ALTERNATIVE2"    : ("%s = %s",          "\n  | "),
    }


def format_slot(s):
    if type(s) is list:
        result = ""
        for node in s:
            formatted_node, separator = format_node(node)
            if result != "":
                result += separator
            result += formatted_node
        return result
    
    if type(s) is tuple:
        return format_node(s)[0]

    text = str(s)
    if re.match("[a-zA-Z0-9_-]*$", text):
        return text

    if not re.search('"', text):
        return '"' + text + '"'

    if not re.search("'", text):
        return "'" + text + "'"
        
    return '"' + text.replace('"', '""') + '"'

def format_node(node):
    type = node[0]
    template, separator = formats[type]

    return (template % tuple([ format_slot(s) for s in slots(node) ]),
            separator)
