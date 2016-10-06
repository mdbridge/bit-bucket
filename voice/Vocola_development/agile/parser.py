###
### Vocola 2.7 parser using PLY
### 

import re
import sys

import yacc

from   lexer import tokens, preprocess_input
import ast



##
## Constructing AST nodes:
## 

current_file = "<stdin>"

def make_loc(line_number):
    return (current_file, line_number)

def make_node(node_type, line_number, *slots):
    return ast.make_node(node_type, make_loc(line_number), *slots)

  # first_nodes: a node or a non-empty list of nodes
def make_compound_node(node_type, first_nodes, *slots):
    if type(first_nodes) is list:
        first_nodes = first_nodes[0]

    return make_node(node_type, ast.loc(first_nodes), *slots)



##
## Handling errors:
## 

def default_error_handler(location, message):
    print >> sys.stderr, ("%s:%s: Error: %s " %
                          (location[0], location[1], message))

error_handler = default_error_handler

def error(location, message):
    error_handler(location, message)



##
## Statements:
## 

def p_maybe_statements(p):
    '''maybe_statements : maybe_statements statement
                        |'''
    if len(p)==1:
        p[0] = []
    else:
        p[0] = p[1] + [p[2]]

def p_statement(p):
    '''statement : definition ';'
                 | function   ';'
                 | command    ';' '''
    p[0] = p[1]


  # <<<>>> import really takes a sequence of actions:
def p_import_statement1(p):
    '''statement : IMPORT quotation ';' '''
    p[0] = make_node("IMPORT", p.lineno(1), p[2][0])

def p_import_statement2(p):
    '''statement : IMPORT RAW ';' '''
    p[0] = make_node("IMPORT", p.lineno(1), p[2])

  # <<<>>>
def p_context_statement(p):
    '''statement : BAD_CONTEXT'''
    p[0] = make_node("CONTEXT", p.lineno(1), p[1])
    

##
## Functions:
## 

#
# Defining:
# 
    
def p_function(p):
    '''function : NAME formals ASSIGN actions '''
    p[0] = make_node('FUNCTION', p.lineno(1), p[1], p[2], p[4])


def make_formal(name, line_number):
    return make_node("FORMAL", line_number, name)

    # can't shift NAME here to avoid conflict with inlined lists:
def p_formals(p):
    '''formals : '('                        ')'
               | '(' NAME                   ')'
               | '(' NAME ',' extra_formals ')' '''
    p[0] = []
    if len(p)>3:
        p[0] += [make_formal(p[2], p.lineno(2))]
    if len(p)>4:
        p[0] += p[4]
            
def p_extra_formals(p):
    '''extra_formals : NAME 
                     | NAME ',' extra_formals'''
    p[0] = [make_formal(p[1], p.lineno(1))]
    if len(p)>2:
        p[0] += p[3]

#
# Calling:
# 

def p_call(p):
    '''call : NAME '(' maybe_arguments ')' '''
    p[0] = make_node("CALL", p.lineno(1), p[1], p[3])
    
def p_maybe_arguments(p):
    '''maybe_arguments : arguments
                       | '''
    if len(p)==1:
        p[0] = []
    else:
        p[0] = p[1]
        
def p_arguments(p):
    '''arguments : argument
                 | argument ',' arguments'''
    p[0] = [p[1]]
    if len(p)>2:
        p[0] += p[3]

def p_argument(p):
    '''argument : actions'''
    p[0] = make_compound_node("ARGUMENT", p[1], p[1])

#
# Actions:
# 
# Note: the non-terminal 'action' actually returns a list of actions
#       because of separating strings into their component parts.
# 

def p_actions(p):
    '''actions : action
               | action actions '''
    p[0] = p[1]
    if len(p)>2:
        p[0] += p[2]

def p_action_call(p):
    '''action : call'''
    p[0] = [p[1]]


def p_action_string0(p):
    '''action : quotation'''
    p[0] = break_up_string(p[1][0], p[1][1])

def p_action_string1(p):
    '''action : RAW
              | NAME'''
    p[0] = break_up_string(p[1], p.lineno(1))

def p_action_string2(p):
    '''action : VARIABLE'''
    p[0] = break_up_string('<' + p[1] + '>', p.lineno(1))
    

def p_action_string3(p):
    '''action : RANGE'''
    p[0] = break_up_string(p[1][0] + '..' + p[1][1], p.lineno(1))


def break_up_string(text, line_number):
    actions = []

    # underscore to space conversion:
    text = re.sub(r'\{(.*?)_(.*?)\}', r'{\1 \2}', text)

    out = ""
    while text != "":
        if text[0:2] == r'\$':
            out += "$"
            text = text[2:]
            continue

        if text[0] == "\\":
            out += text[0:2]
            text = text[2:]
            continue

        m = re.match(r'\$(\d+|[a-zA-Z_]\w*)(.*)', text)
        if not m:
            out += text[0]
            text = text[1:]
            continue

        if out != "":
            actions.append(make_node("STRING", line_number, out))
            out = ""
            
        actions.append(make_node("REF", line_number, m.group(1)))
        text = m.group(2)

    if out != "":
        actions.append(make_node("STRING", line_number, out))

    if len(actions)==0:
        actions.append(make_node("STRING", line_number, ""))
        
    return actions

    
##
## Commands:
## 

def p_command(p):
    "command : terms '=' actions"
    p[0] = make_compound_node("COMMAND", p[1], p[1], p[3])

#
# Terms:
#
# Note: need to avoid reducing NAME until next phrase reduced to avoid
#       conflict with function definitions...
#
#       To do this, split term's into NAME's and other_term's (everything else)
#

def p_maybe_terms(p):
    '''maybe_terms : terms
                   |'''
    if len(p)==1:
        p[0] = []
    else:
        p[0] = p[1]


def word_to_term(text, line_number):
    return make_node("WORD", line_number, text)

def p_terms_name(p):
    '''terms : NAME
             | NAME       terms'''
    p[0] = [word_to_term(p[1], p.lineno(1))]
    if len(p)>2:
        p[0] += p[2]

def p_terms_other(p):
    '''terms : other_term
             | other_term terms'''
    p[0] = [p[1]]
    if len(p)>2:
        p[0] += p[2]


def p_term_raw(p):
    '''other_term : RAW'''
    p[0] = word_to_term(p[1], p.lineno(1))

def p_term_quoted(p):
    '''other_term : quotation'''
    p[0] = word_to_term(p[1][0], p[1][1])


def p_term_variable(p):
    '''other_term : VARIABLE'''
    p[0] = make_node("LIST", p.lineno(1), p[1])

def p_term_range(p):
    '''other_term : RANGE'''
    p[0] = make_node("RANGE", p.lineno(1), int(p[1][0]), int(p[1][1]))

def p_term_optional(p):
    '''other_term : '[' terms ']' '''
    p[0] = make_node("OPTIONAL", p.lineno(1), p[2])

def p_term_inline_list(p):
    '''other_term : inline_list'''
    p[0] = p[1]
            

##
## Lists:
## 

def make_alternative(terms, actions=None):
    if actions:
        return make_node("ALTERNATIVE2", terms, terms, actions)
    else:
        return make_node("ALTERNATIVE",  terms, terms)

#
# Defining separately:
# 

def p_definition(p):
    '''definition : VARIABLE ASSIGN alternatives '''
    p[0] = make_node("LIST-DEFINITION", p.lineno(1), p[1], p[3])


def p_alternatives(p):
    '''alternatives : alternative '|' alternatives
                    | alternative'''
    p[0] = [p[1]]
    if len(p)>2:
        p[0] += p[3]

def p_alternative(p):
    '''alternative : terms
                   | terms '=' actions'''
    if len(p)==2:
        p[0] = make_alternative(p[1])
    else:
        p[0] = make_alternative(p[1], p[3])

#
# Inlined lists:
#
# Note: need to avoid reducing NAME until after had chance to reduce 
#       ( NAME ) to formals...
# 

def make_inline_list(alternatives, line_number):
    return make_node("INLINE-LIST", line_number, alternatives)

def p_inline_name(p):
    '''inline_list : '(' NAME ')' 
                   | '(' NAME terms ')' '''
    terms = [ word_to_term(p[2], p.lineno(2)) ]
    if len(p)>4:
        terms += p[3]
    p[0] = make_inline_list([ make_alternative(terms) ], p.lineno(1))

def p_inline_other(p):
    '''inline_list : '(' other_term maybe_terms ')' '''
    p[0] = make_inline_list([ make_alternative([p[2]] + p[3]) ],
                            p.lineno(1))

def p_inline_command(p):
    '''inline_list : '(' terms '=' actions ')' '''
    p[0] = make_inline_list([ make_alternative(p[2], p[4]) ], p.lineno(1))
    
def p_inline_multiple(p):
    '''inline_list : '(' alternative '|' alternatives ')' '''
    p[0] = make_inline_list([p[2]]+p[4], p.lineno(1))


##
## Quoted words:
##

def p_quotation1(p):
    '''quotation : DOUBLE_QUOTE
                 | SINGLE_QUOTE
                 '''
    p[0] = (p[1][0], p.lineno(1))

def p_quotation2(p):
    '''quotation : BAD_DOUBLE_QUOTE
                 | BAD_SINGLE_QUOTE
                 '''
    p[0] = (p[1][0], p.lineno(1))
    quotation = p[1][1].replace("\n", "")
    error(make_loc(p.lineno(1)), "unterminated quotation: %s" % quotation)



##
## Building the parser:
## 

parser = yacc.yacc()

errors = 0

def parse(text, filename="<stdin>", my_error_handler=default_error_handler):
    global current_file, error_handler, errors

    current_file = filename
    
    errors = 0
    def handler(location, message):
        global errors
        errors += 1
        my_error_handler(location, message)
    error_handler = handler

    text = preprocess_input(text)
    program_ast = parser.parse(text)

    return (program_ast, errors, text)



##
## When run as a stand-alone program:
## 

if __name__ == "__main__":
    if len(sys.argv)==1:
        program_ast, my_errors, dummy = parse(sys.stdin.read())
        print ast.format_slot(program_ast)
    else:
        my_errors = 0
        for filename in sys.argv[1:]:
            print "Compiling %s:" % (filename)
            
            f = open(filename)
            data = f.read()
            program_ast, new_errors, dummy = parse(data, filename)
            my_errors += new_errors
            print ast.format_slot(program_ast)

    print "Total error(s): %s" % (my_errors)
