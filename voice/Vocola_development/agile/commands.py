import sys

from parser import parse
from ast import *


def visit_nodes(ast, visitor):
    if type(ast) is list:
        for node in ast:
            visit_nodes(node, visitor)
    if type(ast) is tuple:
        visitor(ast)
        for slot in ast[1:]:
            visit_nodes(slot, visitor)


def visitor(node):
    if node_type(node) == "COMMAND":
        print format_slot(slot(node, 0))

def process(program):
    visit_nodes(program, visitor)


if len(sys.argv)==1:
    program_ast, my_errors, dummy = parse(sys.stdin.read())
    if my_errors == 0:
        process(program_ast)
else:
    for filename in sys.argv[1:]:
        print "Processing %s:" % (filename)

        f = open(filename)
        data = f.read()
        program_ast, new_errors, dummy = parse(data, filename)
        if new_errors == 0:
            process(program_ast)
