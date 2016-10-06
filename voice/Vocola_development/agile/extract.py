import sys
from parser import *


def visit_nodes(ast, visitor):
    if type(ast) is list:
        for node in ast:
            visit_nodes(node, visitor)
    if type(ast) is tuple:
        visitor(ast)
        for slot in ast[1:]:
            visit_nodes(slot, visitor)
        


def visitor(node):
    if node[0] == "RANGE":
        add_list("%sto%s" % (node[1], node[2]),
                 make_range_list(node[1], node[2]))
    if node[0] == "LIST-DEFINITION":
        add_list(node[1], parse_definition(node[2]))
        

def add_list(name, definition):
    print "need: " + name
    #    print repr(definition)
    print format_DNS_list(definition)

def make_range_list(a, b):
    if a>b:
        a,b = b,a

    result = []
    for i in range(a,b):
        result += [(str(i), None)]

    return result
        

def parse_definition(alternatives):
    # <<<>>>
    if len(alternatives)==1 and alternatives[0][0]=="ALTERNATIVE":
        # only 1 alternative
        only       = alternatives[0]
        terms      = only[1]
        definition = only[2]

        if len(terms)==1 and definition==None:
            # alternative is just 1 term
            term = terms[0]

            if term[0]=="INLINE-LIST":
                alternatives = term[1]

    results = []
    for alternative in alternatives:
        text = parse_terms(alternative[1])
        definition = None
        if alternative[2]:
            definition = parse_actions(alternative[2])
        results +=[(text, definition)]
    
    return results


def parse_terms(terms):
    text = ""
    for term in terms:
        if term[0] == "WORD":
            if text != "":
                text += " "
            text += term[1]
        else:
            print >>sys.stderr,"ERROR: impossible term: " + format_slot(term)
            if text != "":
                text += " "
            text += "impossible_term"
        
    return text

def parse_actions(actions):
    text = ""
    for action in actions:
        if action[0] == "STRING":
            text += action[1]
        else:
            print >>sys.stderr,"ERROR: impossible action: " + format_slot(action)
            
    return text
        





def format_DNS_list(definition):
    results = ""

    for spoken,written in definition:
        if not written:
            results += spoken + "\n"
        else:
            results += written + "\\" +  spoken + "\n"

    return results


def test(node):
    print repr(node) +  format_slot(node)
    
data = sys.stdin.read() + '\n'
data = '\n' + data # hack for include...  <<<>>>
data = data.replace("\r\n", "\n")

ast = parser.parse(data)
visit_nodes(ast, visitor)

