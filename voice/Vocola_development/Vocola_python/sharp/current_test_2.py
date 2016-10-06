import re
import copy
import os


##################################################

# vcl2py:  Convert Vocola voice command files to NatLink Python "grammar"
#          classes implementing the voice commands
#
# Usage:  perl vcl2py.pl [-extensions <f>] [-numbers <s0>,<s1>,<s2>,...]
#                        [-suffix <s>] 
#                        [-f] inputFileOrFolder outputFolder
# Where:
#   -extensions <f> -- specify filename containing extension interface information
#   -numbers <s0>,<s1>,<s2>,...
#                   -- use spoken form <s0> instead of "0" in ranges,
#                                      <s1> instead of "1" in ranges, etc.
#   -suffix <s>     -- use suffix <s> to distinguish Vocola generated files
#                      (default is "_vcl")
#   -f              -- force processing even if file(s) not out of date
#
#
# Copyright (c) 2002-2011 by Rick Mohr.
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
#
#  5/14/2011  ml  Selected numbers in ranges can now be spelled out
# 11/28/2010  ml  Extensions can now be called
# 05/28/2010  ml  Print_* functions -> unparse_* to avoid compiler bug
# 05/08/2010  ml  Underscores now converted to spaces by VocolaUtils
# 03/31/2010  ml  Runtime errors now caught and passed to handle_error along 
#                 with filename and line number of error location
# 01/27/2010  ml  Actions now implemented via direct translation to
#                 Python, with no delay of Dragon calls, etc.
# 01/01/2010  ml  User functions are now implemented via unrolling
# 12/30/2009  ml  Eval is now implemented via transformation to EvalTemplate
# 12/28/2009  ml  New EvalTemplate built-in function
# 09/06/2009  ml  New $set directive replaces old non-working sequence directive
#                 binary Use Command Sequences replaced by n-ary MaximumCommands
# 01/19/2009  ml  Unimacro built-in added
# 12/06/2007  ml  Arguments to Dragon functions are now checked for proper 
#                 number and datatype
# 06/02/2007  ml  Output filenames are now mangled in an invertable fashion
# 05/17/2007  ml  Eval now works correctly on any action instead of just word
#                 and reference actions.
# 05/15/2007  ml  Variable substitution regularized
#                 Empty context statements now work
# 04/18/2007  ml  (Function) Names may now start with underscores
# 04/08/2007  ml  Quotation marks can be escaped by doubling
# 01/03/2005  rm  Commands can incorporate arbitrary dictation 
#                 Enable/disable command sequences via ini file
# 04/12/2003  rm  Case insensitive window title comparisons
#                 Output e.g. "emacs_vcl.py" (don't clobber existing NatLink 
#                 files)
# 11/24/2002  rm  Option to process a single file, or only changed files
# 10/12/2002  rm  Use <any>+ instead of exporting individual NatLink commands
# 10/05/2002  rm  Generalized indenting, emit()
# 09/29/2002  rm  Built-in function: Repeat() 
# 09/15/2002  rm  User-defined functions
# 08/17/2002  rm  Use recursive grammar for command sequences
# 07/14/2002  rm  Context statements can contain '|'
#                 Support environment variable references in include statements
# 07/06/2002  rm  Function arguments allow multiple actions
#                 Built-in function: Eval()!
# 07/05/2002  rm  New code generation using VocolaUtils.py
# 07/04/2002  rm  Improve generated code: use "elif" in menus
# 06/02/2002  rm  Command sequences!
# 05/19/2002  rm  Support "include" statement
# 05/03/2002  rm  Version 1.1
# 05/03/2002  rm  Handle application names containing '_'
# 05/03/2002  rm  Convert '\' to '\\' early to avoid quotewords bug
# 02/18/2002  rm  Version 0.9
# 12/08/2001  rm  convert e.g. "{Tab_2}" to "{Tab 2}"
#                 expand in-string references (e.g. "{Up $1}")
# 03/31/2001  rm  Detect and report unbalanced quotes
# 03/06/2001  rm  Improve error checking for complex menus
# 02/24/2001  rm  Change name to Vocola
# 02/18/2001  rm  Handle terms containing an apostrophe
# 02/06/2001  rm  Machine-specific command files
# 02/04/2001  rm  Error on undefined variable or reference out of range
# 08/22/2000  rm  First usable version

# Style notes:
#   Global variables are capitalized (e.g. $Definitions)
#   Local variables are lowercase (e.g. $in_folder)




def read_extensions_file(extensions_file):
    global Debug, Extension_functions, LOG
    if (Debug >= 1): print >>LOG, "extensions file is '" + extensions_file
    try:
        input = open(extensions_file) ##=         input = File.OpenRead(extensions_file) 
        for line in input:
            match = re.match(r'([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,\n\r]*)[\n\r]*$', line)
            if not match:
                continue

            extension_name    = match.group(1)
            minimum_arguments = int(match.group(2))
            maximum_arguments = int(match.group(3))
            needs_flushing    = int(match.group(4)) != 0
            module_name       = match.group(5)
            function_name     = match.group(6)
            
            Extension_functions[extension_name] = [minimum_arguments, maximum_arguments, needs_flushing, module_name, function_name]

    except IOError, e:
        return



# Convert one Vocola command file to a .py file

def convert_file(in_file, out_folder, suffix):
    global Debug, Definitions, Error_count, Error_encountered, File_empty, Force_processing, Forward_references, Function_definitions, Functions, In_folder, Include_stack, Included_files, Input_name, Module_name, Number_words, Should_emit_dictation_support, Statement_count, default_maximum_commands, maximum_commands
    out_file = convert_filename(in_file)
    
    # The global $Module_name is used below to implement application-specific 
    # commands in the output Python
    Module_name = out_file.lower()
    # The global $Input_name is used below for error logging
    Input_name = in_file + ".vcl"
    Should_emit_dictation_support = 0
    out_file = out_folder + "/" + out_file + suffix + ".py"
    
    # in_stats  = stat(In_folder + "/" + Input_name)
    # out_stats = stat(out_file)
    # in_date   = in_stats->mtime
    # out_date  = out_stats ? out_stats->mtime : 0
    # if not in_date > out_date or Force_processing: return
    
    Definitions          = {}
    Functions            = {}
    Function_definitions = {}
    
    Forward_references   = []
    Included_files       = []
    Include_stack        = []
    Error_count          = 0
    File_empty           = True
    Statement_count      = 1
    
    if Debug>=1: print >>LOG, "\n=============================="
    
    statements = parse_file(Input_name)
    check_forward_references()
    
    # Prepend a "global" context statement if necessary
    if len(statements) == 0 or statements[0]["TYPE"] != "context": 
        context = {}
        context["TYPE"] = "context"
        context["STRINGS"] = [""]
        statements.insert(0, context)
    #print >>LOG, unparse_statements (@statements),
    transform_nodes(statements)
    #print >>LOG, unparse_statements (@statements),
    print unparse_statements(statements),
    
    # Handle $set directives:
    maximum_commands = default_maximum_commands
    for statement in statements: 
        if statement["TYPE"] == "set": 
            key = statement["KEY"]
            if key == "MaximumCommands": 
                maximum_commands = int(statement["TEXT"])
            elif key == "numbers": 
                Number_words = {}
                numbers = re.split(r'\s*,\s*', statement["TEXT"])
                i = 0
                for number in numbers: 
                    Number_words[i] = number
                    i = i + 1
    if Error_count:  ##= if Error_count != 0:
        if Error_count == 1:
            s = ""
        else:
            s = "s"
        print >>LOG, "  " + str(Error_count) + " error" + s + " -- file not converted."
        Error_encountered = 1
        return
    if File_empty: 
        # Write empty output file, for modification time comparisons 
        OUT = open(out_file, "w") ##= OUT = File.OpenWrite(out_file)
        #open OUT, ">" + out_file or die "$@ " + out_file + "\n"
        OUT.close()
        print >>LOG, "Converting " + Input_name
        print >>LOG, "  Warning: no commands in file."
        return
    #emit_output(out_file, statements)

# 
# Warning: this code is very subtle and has a matching inverse function in 
# _vocola_main.py, getSourceFilename.
#
# Ensures:
#   maps [\w@]* to [\w]*, [-\w@]* to [-\w]*
#   is invertable
#   result starts with _ iff input did
#   does not change any text before the first @ or end of string, whichever 
#     comes first
# 
def convert_filename(in_file):
    name = in_file
    
    # ensure @ acts as a module name terminator for NatLink
    name = re.sub(r'(.)@', r'\1_@', name)
    
    marker = "e_s_c_a_p_e_d__"

    match = re.match(r'([^@]*?)((@(.*))?)$', name)
    module = match.group(1)
    suffix = match.group(2)
    
    if suffix == "" and name.find(marker) == -1: return name
    
    suffix = suffix.replace('_','___')
    suffix = suffix.replace('@','__a_t__')
    return module + marker + suffix

# ---------------------------------------------------------------------------
# Parsing routines
#
# The following grammar defines the Vocola language:
# (note that a "menu" is called an "alternative set" in the documentation)
#
#     statements = (context | definition | function | directive | top_command)*
#
#        context = chars* ('|' chars*)* ':'
#     definition = variable ':=' menu_body ';'
#       function = prototype ':=' action* ';'
#      directive = ('include' word | '$set' word word) ';'
#    top_command = terms '=' action* ';'
#
#        command = terms ['=' action*]
#          terms = (term | '[' simple_term ']')+
#           term = simple_term | range | menu
#    simple_term = word | variable
#         action = word | call | reference
#
#           menu = '(' menuBody ')'
#       menuBody = command ('|' command)*
#
#      prototype = functionName '(' formals ')'
#        formals = [name (',' name)*]
#           call = callName '(' arguments ')'
#      arguments = [action* (',' action*)*]
#
#
#  [lexing]
#
#           word = bare_word | quoted_word
#    quoted_word = '"' ([^"]|'""')* '"' | "'" ([^']|"''")* "'"
#      bare_word = [^\s()\[\]=|,\"\';\#]*([^\s()\[\]=|,\"\';\#:]|:(?![\s=]))
#
#       variable = '<' variableName '>'
#          range = number '..' number
#      reference = '$' (number | name)
#           name = [a-zA-Z_]\w*
#   variableName = \w+
#   functionName = [a-zA-Z_]\w*
#       callName = [a-zA-Z_][\w.]*
#
#
# The parser works as follows:
#     1) Strip comments
#     2) Find statement segments by slicing at major delimiters (: ; :=)
#     3) Parse each segment using recursive descent
#
# The parse tree is built from three kinds of nodes (statement, term, 
# and action), using the following fields:
#
# statement: 
#    TYPE - command/definition/function/context/include/set
#    command:
#       NAME    - unique number
#       TERMS   - list of "term" structures
#       ACTIONS - list of "action" structures
#       LINE    - last line number of command if it is a top-level command
#       FILE    - filename of file containing command
#    definition:
#       NAME    - name of variable being defined
#       MENU    - "menu" structure defining alternatives
#    function:
#       NAME    - name of function being defined
#       FORMALS - list of argument names
#       ACTIONS - list of "action" structures
#    context:
#       STRINGS - list of strings to use in context matching;
#                 the list ("") denotes the noop context restriction (:)
#       RULENAMES - list of rule names defined for this context
#    include:
#       TEXT    - filename being included
#    set:
#       KEY     - key being set
#       TEXT    - value to set the key to
# 
# term:
#    TYPE   - word/variable/range/menu/dictation
#    NUMBER - sequence number of this term
#    word:
#       TEXT     - text defining the word(s)
#       OPTIONAL - is this word optional
#    variable:
#       TEXT     - name of variable being referenced
#       OPTIONAL - is this variable optional
#    range:
#       FROM     - start number of range
#       TO       - end number of range
#    menu:
#       COMMANDS - list of "command" structures defining the menu
#       
# action:
#    TYPE - word/reference/formalref/call
#    word:
#       TEXT      - keystrokes to send
#    reference:
#       TEXT      - reference number (a string) of reference referenced
#    formalref:
#       TEXT      - name of formal (i.e. user function argument) referenced
#    call:
#       TEXT      - name of function called
#       CALLTYPE  - dragon/vocola/user/extension
#       ARGTYPES  - [dragon only] types of call arguments
#       ARGUMENTS - list of lists of actions, to be passed in call

# ---------------------------------------------------------------------------
# Built in Vocola functions with (minimum number of arguments, maximum
# number of arguments):

Vocola_functions = {}  ##: Dictionary<string,int[]>
#                     "Eval"              : [1,1],
#                     "EvalTemplate"      : [1,-1],
#                     "Repeat"            : [2,2],
#                     "Unimacro"          : [1,1],
#                   }

# Vocola extensions with (extension_name, minimum_arguments, maximum_arguments,
# needs_flushing, module_name, function_name); initialized by 
# read_extensions_file():

Extension_functions = {}  ##: Dictionary<string,List<dynamic>>

# Built in Dragon functions with (minimum number of arguments,
# template of types of all possible arguments); template has one
# letter per argument with s denoting string and i denoting integer:

Dragon_functions = {}  ##: Dictionary<string,List<dynamic>>
#                     "ActiveControlPick" : [1,"s"],
#                     "ActiveMenuPick"    : [1,"s"],
#                     "AppBringUp"        : [1,"ssis"],
#                     "AppSwapWith"       : [1,"s"],
#                     "Beep"              : [0,""],
#                     "ButtonClick"       : [0,"ii"],
#                     "ClearDesktop"      : [0,""],
#                     "ControlPick"       : [1,"s"],
#                     "DdeExecute"        : [3,"sssi"],
#                     "DdePoke"           : [4,"ssss"],
#                     "DllCall"           : [3,"sss"],
#                     "DragToPoint"       : [0,"i"],
#                     "GoToSleep"         : [0,""],
#                     "HeardWord"         : [1,"ssssssss"],  # max 8 words
#                     "HTMLHelp"          : [2,"sss"],
#                     "MenuCancel"        : [0,""],
#                     "MenuPick"          : [1,"s"],
#                     "MouseGrid"         : [0,"ii"],
#                     "MsgBoxConfirm"     : [3,"sis"],
#                     "PlaySound"         : [1,"s"],
#                     "RememberPoint"     : [0,""],
#                     "RunScriptFile"     : [1,"s"],
#                     "SendKeys"          : [1,"s"],
#                     "SendDragonKeys"    : [1,"s"],
#                     "SendSystemKeys"    : [1,"si"],
#                     "SetMicrophone"     : [0,"i"],
#                     "SetMousePosition"  : [2,"iii"],
#                     "SetNaturalText"    : [1,"i"],
#                     "ShellExecute"      : [1,"siss"],
#                     "ShiftKey"          : [0,"ii"],
#                     "TTSPlayString"     : [0,"ss"],
#                     "Wait"              : [1,"i"],
#                     "WaitForWindow"     : [1,"ssi"],
#                     "WakeUp"            : [0,""],
#                     "WinHelp"           : [2,"sii"],
#                    }

# parse_file returns a parse tree (list of statements), which includes in-line
# any statements from include files. Since parse_file is called recursively
# for include files, all code applying to the parse tree as a whole is
# executed in this routine.

def parse_file(in_file):    # returns a list of statements
    global In_folder, Include_stack, Included_files, Offset
    
    Included_files.append(in_file)
    Include_stack.append(in_file)
    #    in_file = In_folder + "\\" + in_file
    #in_file =  "source/" + in_file  # <<<>>>

    text = read_file(in_file)
    open_text(text)
    try:
        statements = parse_statements()
    finally:
        close_text()

    Include_stack.pop()
    return statements

def read_file(in_file):     
    if re.search('/', in_file) == None: in_file = "source/" + in_file  #<<<>>>
    try:
        return open(in_file).read() ##=         return File.OpenRead(in_file).ReadAllText()
    except IOError, e:
        log_error("Unable to open or read '" + in_file + "'")
        return ""

# This is the main parsing loop.

def parse_statements():    # statements = (context | top_command | definition)*
    global Definitions, Formals, Include_stack, Statement_count, Variable_terms, Offset, Tokens

    statements = []
    while not peek(TOKEN_EOF):
        Variable_terms = []  # used in error-checking
        Formals        = []
        starting_position = get_current_position()
        try:
            statement = parse_statement()
        except SyntaxError, e:
            log_error(str(e))
            Offset += 1
            # panic until after next ";":
            while not peek(TOKEN_EOF) and not peek(TOKEN_SEMICOLON):
                try:
                    eat(Tokens[Offset][0])
                except SyntaxError, e2:
                    log_error(str(e2))
                    Offset += 1
            if peek(TOKEN_SEMICOLON):
                eat(TOKEN_SEMICOLON)
            continue
        except RuntimeError, e:
            log_error(str(e))
            # panic until after next TOKEN_SEMICOLON:
            while not peek(TOKEN_EOF) and not peek(TOKEN_SEMICOLON):
                try:
                    eat(Tokens[Offset][0])
                except SyntaxError, e2:
                    log_error(str(e2))
                    Offset += 1
            if peek(TOKEN_SEMICOLON):
                eat(TOKEN_SEMICOLON)
            continue

        if statement["TYPE"] == "definition": 
            name = statement["NAME"]
            if Definitions.has_key(name): log_error("Redefinition of <"+name+">")
            Definitions[name] = statement
        elif statement["TYPE"] == "command": 
            statement["NAME"] = str(Statement_count)
            Statement_count += 1

        #print unparse_statements([statement]),
        if statement["TYPE"] != "include": 
            statements.append(statement)
        else: 
            # Handle include file
            include_file = expand_variables(statement["TEXT"])
            if not already_included(include_file):
                # Save context, get statements from include file, restore 
                Include_stack.extend([get_line_number(starting_position), None])  # <<<>>>
                #print "--> " + include_file
                statements.extend(parse_file(include_file))
                #print "<--"
                Include_stack.pop()
                Include_stack.pop()

    return statements

def parse_statement():
    if peek(TOKEN_CONTEXT):
        return parse_context()
    if peek(TOKEN_LBRACKET|TOKEN_LPAREN|TOKEN_SINGLE_WORD|TOKEN_DOUBLE_WORD):
        return parse_top_command()


    start = get_current_position()
    eat(TOKEN_BARE_WORD)

    # b ^ :=
    if peek(TOKEN_COLON_EQUALS):
        rewind(start); return parse_variable_definition()

    # b ^ w{0,2} ;  or  b ^ w{2} <term>  or b ^ w{0,2} =
    directive_arguments = 0
    while peek(TOKEN_BARE_WORD|TOKEN_SINGLE_WORD|TOKEN_DOUBLE_WORD):
        eat_any()
        directive_arguments += 1
        if directive_arguments > 2:
            rewind(start); return parse_top_command()
    if peek(TOKEN_SEMICOLON):
        rewind(start); return parse_directive()
    if directive_arguments > 0:
        if not peek(TOKEN_LBRACKET|TOKEN_LPAREN):
            eat(TOKEN_EQUALS)
        rewind(start); return parse_top_command()

    # b ^ <term> in unambiguous case  or  b ^ =
    if peek(TOKEN_LBRACKET|TOKEN_SINGLE_WORD|TOKEN_DOUBLE_WORD|TOKEN_EQUALS):
        rewind(start); return parse_top_command()


    eat(TOKEN_LPAREN)
    # b( ^ <term>  or  b( ^ )  or  b( ^ b)  or  b( ^ b,  or  b( ^ b) :=
    if peek(TOKEN_RPAREN):
        rewind(start); return parse_function_definition()
    if peek(TOKEN_LBRACKET|TOKEN_SINGLE_WORD|TOKEN_DOUBLE_WORD|TOKEN_LPAREN):
        rewind(start); return parse_top_command()


    eat(TOKEN_BARE_WORD)
    # b(b ^ <term>  or  b(b ^ |  or b(b ^ =  or  b(b ^ )  or  b(b ^ ,
    if peek(TOKEN_TERM|TOKEN_BAR|TOKEN_EQUALS):
        rewind(start); return parse_top_command()
    if peek(TOKEN_COMMA):
        rewind(start); return parse_function_definition()
    
    eat(TOKEN_RPAREN)
    # b(b) ^ <term>  or  b(b) ^ =  or b(b) ^ :=
    if peek(TOKEN_TERM|TOKEN_EQUALS):
        rewind(start); return parse_top_command()


    eat(TOKEN_COLON_EQUALS)
    rewind(start); return parse_function_definition()

def parse_context():    # context = chars* ('|' chars*)* ':'
    global Debug, LOG
    statement = {}
    statement["TYPE"] = "context"
    raw = eat(TOKEN_CONTEXT)[1][:-1]
    raw = raw.strip().lower()  ##=     raw = raw.Trim().ToLower() 
    strings = re.split(r'\s*\|\s*', raw)
    if len(strings)== 0:
        strings = [""]
    statement["STRINGS"] = strings
    if Debug>=1: print >>LOG, unparse_directive (statement),
    return statement

def parse_variable_definition():    # definition = variable ':=' menu_body ';'
    global Debug, LOG
    name = eat(TOKEN_BARE_WORD)[1]
    if not re.match(r'<.*>$', name):
        die("Illegal variable '" + name + "': variables must start with '<' and end with '>'\n")
    name = name[1:-1]
    check_variable_name(name)
    if name == "_anything":
        die("Built-in list <_anything> is not redefinable\n")

    statement = {}
    statement["TYPE"] = "definition"
    statement["NAME"] = name
    eat(TOKEN_COLON_EQUALS)
    menu = parse_menu_body(TOKEN_SEMICOLON)
    eat(TOKEN_SEMICOLON)
    verify_referenced_menu(menu)
    statement["MENU"] = menu
    if Debug>=1: print >>LOG, unparse_definition (statement),
    return statement

def check_variable_name(name):
    if not re.match(r'\w+$', name):
        die("Illegal variable name: <" + name + ">\n")

def parse_function_definition():   # function = prototype ':=' action* ';'
                                   # prototype = functionName '(' formals ')'
    global Debug, Formals, Function_definitions, Functions, LOG
    functionName = eat(TOKEN_BARE_WORD)[1]
    if Debug>=2: print >>LOG, "Found user function:  " + functionName + "()"
    if not re.match(r'[a-zA-Z_]\w*$', functionName):
        die("illegal user function name: " + functionName + "\n")

    eat(TOKEN_LPAREN)
    formals = parse_formals()
    eat(TOKEN_RPAREN)
    
    statement            = {}
    statement["TYPE"]    = "function"
    statement["NAME"]    = functionName
    statement["FORMALS"] = formals
    Formals              = formals # Used below in parse_formal_reference

    eat(TOKEN_COLON_EQUALS)
    statement["ACTIONS"] = parse_actions(TOKEN_SEMICOLON)
    eat(TOKEN_SEMICOLON)

    if Functions.has_key(functionName):
        die("Redefinition of " + functionName + "()\n")
    Functions[functionName] = len(formals)  # remember number of formals
    Function_definitions[functionName] = statement
    if Debug>=1: print >>LOG, unparse_function_definition (statement),
    return statement

def parse_formals():    # formals = [name (',' name)*]
    global Debug, LOG
    safe_formals = []
    if not peek(TOKEN_RPAREN):
        while True:
            formal = eat(TOKEN_BARE_WORD)[1]
            if not re.match(r'[a-zA-Z_]\w*$', formal):
                die("Illegal formal name: '" + formal + "'\n")
            if Debug>=2: print >>LOG, "Found formal:  " + formal
            safe_formals.append("_" + formal)
            if peek(TOKEN_COMMA): 
                eat(TOKEN_COMMA)
            else:
                break
    return safe_formals

def parse_top_command():    # top_command = terms '=' action* ';'
    global Debug, File_empty, LOG
    statement = parse_command(TOKEN_SEMICOLON, True)
    eat(TOKEN_SEMICOLON)
    File_empty = False
    if Debug>=1: print >>LOG, unparse_command (statement, True)
    return statement

def parse_directive():    # directive = ('include' word | '$set' word word) ';'
    global Debug, LOG

    statement = {}
    directive = eat(TOKEN_BARE_WORD)[1]
    if directive == "include":
        statement["TYPE"] = "include"
        word = parse_word()
        statement["TEXT"] = word["TEXT"]
        eat(TOKEN_SEMICOLON)
    elif directive == "$set":
        statement["TYPE"] = "set"
        word = parse_word()
        statement["KEY"] = word["TEXT"]
        word = parse_word()
        statement["TEXT"] = word["TEXT"]
        eat(TOKEN_SEMICOLON)
    else:
        die("Invalid directive name: " + directive)

    if Debug>=1: print >>LOG, unparse_directive (statement),
    return statement

def parse_command(separators, needs_actions=False): # command = terms ['=' action*]
    global Include_stack, Variable_terms
    if needs_actions:
        terms = parse_terms(TOKEN_EQUALS)
    else:
        terms = parse_terms(separators | TOKEN_EQUALS)
    
    command = {}
    command["TYPE"] = "command"
    last = len(Include_stack)
    file = Include_stack[last-1]
    command["FILE"] = file
    command["TERMS"] = terms
    
    # Count variable terms for range checking in parse_reference
    Variable_terms = get_variable_terms(command)
    
    if needs_actions or peek(TOKEN_EQUALS):
        eat(TOKEN_EQUALS)
        command["ACTIONS"] = parse_actions(separators)

    command["LINE"] = get_line_number(get_current_position()) # line number is *last* line of command
    return command

def parse_terms(separators):    # terms = (term | '[' simple_term ']')+
    error = True
    terms = []
    while True:
        if peek(TOKEN_LBRACKET):
            optional = True
            eat(TOKEN_LBRACKET)
            term = parse_simple_term()
            eat(TOKEN_RBRACKET)
        else:
            optional = False
            term = parse_term()

        term["OPTIONAL"] = optional
        if (not optional and term["TYPE"] != "dictation"): error = False
        terms.append(term)

        if peek(separators): 
            break

    if error: 
        die("At least one term must not be optional or <_anything>\n")
    else: 
        return combine_terms(terms)

def combine_terms(terms):   # Combine adjacent "word" terms; number resulting terms ##: List<Node>
    new_terms = []
    term_count = 0
    i = 0
    while i < len(terms):
        term = terms[i]
        i += 1

        if is_required_word(term): 
            while i<len(terms) and is_required_word(terms[i]): 
                term["TEXT"] += " " + terms[i]["TEXT"]
                i += 1
        term["NUMBER"] = term_count
        term_count += 1
        new_terms.append(term)

    return new_terms

def is_required_word(term): 
    return term["TYPE"] == "word" and not term["OPTIONAL"]

def parse_term():         #  term = simple_term | range | menu
    global Debug, LOG     # range = number '..' number
    peek(TOKEN_TERM)
    if peek(TOKEN_LPAREN):         #  menu = '(' menuBody ')'
        eat(TOKEN_LPAREN)
        term = parse_menu_body(TOKEN_RPAREN)
        eat(TOKEN_RPAREN)
        if Debug>=2: 
            print >>LOG, "Found menu:  " + unparse_menu (term, True)
        return term
    
    bare = peek(TOKEN_BARE_WORD)
    term = parse_simple_term()
    if bare and term["TYPE"] == "word":
        word = term["TEXT"]
        match = re.match(r'(\d+)\.\.(\d+)$', word)
        if match:
            term = {}
            term["TYPE"] = "range"
            term["FROM"] = int(match.group(1))
            term["TO"]   = int(match.group(2))
            if Debug>=2: print >>LOG, "Found range:  " + match.group(1) + ".." + match.group(2)
    
    return term

def parse_simple_term():            # simple_term = word | variable
    global Debug, Definitions, LOG  #    variable = '<' name '>'
    word_node = parse_word()
    word = word_node["TEXT"]
    match = re.match(r'<(.*?)>', word)
    if match:
        name = match.group(1)
        check_variable_name(name)
        if name == "_anything": 
            if Debug>=2: print >>LOG, "Found <_anything>"
            return create_dictation_node()
        else: 
            if Debug>=2: print >>LOG, "Found variable:  <" + name + ">"
            if not Definitions.has_key(name): add_forward_reference(name)
            return create_variable_node(name)
    else: 
        return word_node

def create_dictation_node():
    global Should_emit_dictation_support
    Should_emit_dictation_support = True
    term = {}
    term["TYPE"] = "dictation"
    return term

def create_variable_node(name):
    term = {}
    term["TYPE"] = "variable"
    term["TEXT"] = name
    return term

def parse_menu_body(separators):    # menuBody = command ('|' command)*
    commands = []
    while True: 
        if peek(separators):
            die("Empty alternative set\n")
        commands.append(parse_command(separators | TOKEN_BAR))
        if peek(TOKEN_BAR):
            eat(TOKEN_BAR)
        else:
            break

    menu = {}
    menu["TYPE"] = "menu"
    menu["COMMANDS"] = commands
    return menu

def parse_actions(separators):    # action = word | call | reference
    actions = []
    while not peek(separators):
        peek(TOKEN_ACTION)
        bare      = peek(TOKEN_BARE_WORD)
        word_node = parse_word()
        word      = word_node["TEXT"]

        if bare and peek(TOKEN_LPAREN):
            actions.append(parse_call(word))
            continue

        if word == "":
            actions.append(word_node)
            continue

        # expand in-string references (e.g. "{Up $1}") and unquote 
        # $'s (e.g., \$ -> $)
        while word != "":
            # reference = '$' (number | name)
            match = re.search(r'(?<!\\)\$(?:(\d+)|([a-zA-Z_]\w*))', word)
            if match:
                before = word[0: match.start(0)]
                if before != "":
                    actions.append(create_word_node(before, True))
                if match.group(1) != None:
                    actions.append(create_reference_node(match.group(1)))
                else:
                    actions.append(create_formal_reference_node(match.group(2)))
                word = word[match.end(0):]
            else:
                actions.append(create_word_node(word, True))
                break
    
    return actions

def create_reference_node(n):
    global Debug, Variable_terms, LOG
    if int(n) > len(Variable_terms): die("Reference '$" + n + "' out of range\n")
    term = Variable_terms[int(n) - 1]
    if term["TYPE"] == "menu": verify_referenced_menu(term)
    if Debug>=2: print >>LOG, "Found reference:  $" + n
    action = {}
    action["TYPE"] = "reference"
    action["TEXT"] = n
    return action

def create_formal_reference_node(name):
    global Debug, Formals, LOG
    formal = "_" + name
    if formal not in Formals: ##= if not Formals.Contains(formal):
        die("Reference to unknown formal '$" + name + "'\n")
    if Debug>=2: print >>LOG, "Found formal reference:  $" + name
    action = {}
    action["TYPE"] = "formalref"
    action["TEXT"] = formal
    return action

def parse_call(callName):    # call = callName '(' arguments ')'
    global Debug, Dragon_functions, Extension_functions, Functions, Vocola_functions, LOG
    if Debug>=2: print >>LOG, "Found call:  " + callName + "()"
    if not re.match(r'[\w.]+$', callName):
        die("Illegal function call name: '" + callName + "'")

    action = {}
    action["TYPE"] = "call"
    action["TEXT"] = callName
    eat(TOKEN_LPAREN)
    action["ARGUMENTS"] = parse_arguments()
    eat(TOKEN_RPAREN)
    
    nActuals = len(action["ARGUMENTS"])
    if callName.find(".") != -1:
        if Extension_functions.has_key(callName): 
            callFormals = Extension_functions[callName]
            lFormals = callFormals[0]
            uFormals = callFormals[1]
            action["CALLTYPE"] = "extension"
        else: 
            die("Call to unknown extension '" + callName + "'\n")
    elif Dragon_functions.has_key(callName):
        callFormals = Dragon_functions[callName]
        lFormals =     callFormals[0]
        uFormals = len(callFormals[1])
        action["CALLTYPE"] = "dragon"
        action["ARGTYPES"] = callFormals[1]
    elif Vocola_functions.has_key(callName):
        callFormals = Vocola_functions[callName]
        lFormals = callFormals[0]
        uFormals = callFormals[1]
        action["CALLTYPE"] = "vocola"
    elif Functions.has_key(callName): 
        lFormals = uFormals = Functions[callName]
        action["CALLTYPE"] = "user"
    else: 
        die("Call to unknown function '" + callName + "'\n")

    if lFormals != -1 and nActuals < lFormals: 
        die("Too few arguments passed to '" + callName + "' (minimum of " + str(lFormals) + " required)\n")
    if uFormals != -1 and nActuals > uFormals: 
        die("Too many arguments passed to '" + callName + "' (maximum of " + str(uFormals) + " allowed)\n")

    return action

def parse_arguments():    # arguments = [action* (',' action*)*]
    arguments = []
    if not peek(TOKEN_RPAREN):
        while True:
            arguments.append(parse_actions(TOKEN_COMMA|TOKEN_RPAREN))
            if peek(TOKEN_COMMA): 
                eat(TOKEN_COMMA)
            else:
                break

    return arguments

# word      = bare_word | '"' ([^"]|'""')* '"' | "'" ([^']|"''")* "'"
# bare_word = [^\s()\[\]=|,\"\';\#]*([^\s()\[\]=|,\"\';\#:]|:(?![\s=]))
def parse_word():    
    global Debug, LOG
    if peek(TOKEN_DOUBLE_WORD):
        word = eat(TOKEN_DOUBLE_WORD)[1][1:-1].replace('""', '"')
    elif peek(TOKEN_SINGLE_WORD):
        word = eat(TOKEN_SINGLE_WORD)[1][1:-1].replace("''", "'")
    else:
        word = eat(TOKEN_BARE_WORD)[1]
    if Debug>=2: print >>LOG, "Found word:  '" + word + "'"
    return create_word_node(word, False)

def create_word_node(text, substitute):
    if substitute: text = text.replace(r'\$','$')  # convert \$ to $
    term = {}
    term["TYPE"] = "word"
    term["TEXT"] = text
    return term

def log_error(message):
    global Error_count, Input_name, LOG
    # a variant of this code may be found in check_forward_references
    if not Error_count: print >>LOG, "Converting " + Input_name ##=     if Error_count== 0: print >>LOG, "Converting " + Input_name 
    print >>LOG, format_error_message(message),
    Error_count += 1

# Here is what the include stack looks like (growing downwards): 
#   name of top-level file 
#   line number of first include
#   segments pending after first include
#   name of first include file
#   line number of second include
#   segments pending after second include
#   name of second include file

def format_error_message(message):
    global Include_stack
    if message[-1] == "\n":
        message = message[:-1]
    last = len(Include_stack)-1
    line = get_line_number(get_current_position())
    if line == -1:   # <<<>>>
        # "Unable to open file" -- ignore top frame of include stack
        line = Include_stack[last - 2]
        last -= 3
    if last <= 0:
        file_msg = ""
    else:
        file_msg = " of " + Include_stack[last]
    message = "  Error at line " + str(line) + "" + file_msg + ":  " + message + "\n"
    indent = ""
    i = last
    while i >= 3:
        line = Include_stack[i - 2]
        file = Include_stack[i - 3]
        indent += "  "
        message += indent + "  (Included at line " + str(line) + " of " + file + ")\n"
        i -= 3
    return message   

def already_included(filename):
    global Included_files
    # Return TRUE if filename was already included in the current file
    return filename in Included_files ##= return Included_files.Contains(filename)

def expand_variables(text):
    result = ""
    while text != "":
        match = re.search(r'(?<!\\)\$(\d+|[a-zA-Z_]\w*)', text)
        if match:
            result += text[0: match.start(0)].replace(r'\$', '$')
            variable = match.group(1)
            value = os.environ.get(variable) ##= value = "" # <<<>>>
            if not value:
                log_error("Reference to unknown environment variable " + variable) 
            else:
                result += value
            text = text[match.end(0):]
        else:
            result += text.replace(r'\$', '$')
            break
    return result
    # Should be a warning not an error.

# ---------------------------------------------------------------------------
# Parse-time error checking of references

def verify_referenced_menu(menu, parent_has_actions=False, parent_has_alternatives=False):
    commands = menu["COMMANDS"]
    for command in commands: 
        has_actions = parent_has_actions
        if command.has_key("ACTIONS"): 
            if parent_has_actions: die("Actions may not be nested\n")
            actions = command["ACTIONS"]
            has_actions = True
            # make sure no actions are references
            for action in actions: 
                if action["TYPE"] == "reference": 
                    die("Substitution may not contain a reference\n")
        terms = command["TERMS"]
        if len(terms) > 1: die("Alternative is too complex\n")
        has_alternatives = parent_has_alternatives
        if len(commands) != 1: 
            has_alternatives = True
        type = terms[0]["TYPE"]
        if    type == "menu": verify_referenced_menu(terms[0],has_actions,
                                                     has_alternatives)
        elif type == "variable" or type == "definition": 
            die("Alternative cannot be a variable\n")
        elif type == "range": 
            # allow a single range with no actions if it is the only
            # alternative in the (nested) set:
            if (not has_actions and not has_alternatives): return
            die("Range may not be an alternative to something else or have actions\n")

def add_forward_reference(variable):
    global Forward_references
    forward_reference = {}
    forward_reference["VARIABLE"] = variable
    forward_reference["MESSAGE"] = \
       format_error_message("Reference to undefined variable '<" + variable + ">'\n")
    Forward_references.append(forward_reference)

def check_forward_references():
    global Definitions, Error_count, Forward_references, Input_name, LOG
    for forward_reference in Forward_references: 
        variable = forward_reference["VARIABLE"]
        if not Definitions.has_key(variable):
            if not Error_count: print >>LOG, "Converting " + Input_name ##=             if Error_count== 0: print >>LOG, "Converting " + Input_name 
            print >>LOG, forward_reference["MESSAGE"]
            Error_count += 1

# ---------------------------------------------------------------------------
# Unparsing of data structures (for debugging and generating error messages)

def unparse_statements(statements):
    result = ""
    for statement in statements: 
        type = statement["TYPE"]
        if type == "context" or type == "include" or type == "set": 
            result += unparse_directive (statement)
        elif type == "definition": 
            result += unparse_definition (statement)
        elif type == "function": 
            result += unparse_function_definition (statement)
        elif type == "command": 
            result +=  "C" + statement["NAME"] + ":  "
            result += unparse_command (statement, True) + ";\n"
    return result + "\n"

def unparse_directive(statement):
    type = statement["TYPE"]
    if type == "set": 
        return "$set '" + statement["KEY"] + "' to '" + statement["TEXT"] + "'\n"
    elif type == "context":   # bug fix <<<>>>
        return "|".join(statement["STRINGS"]) + ":\n"
    else: 
        return statement["TYPE"] + ":  '" + statement["TEXT"] + "'\n"

def unparse_definition(statement):
    return "<" + statement["NAME"] + "> := " + unparse_menu (statement["MENU"], True) + ";\n"

def unparse_function_definition(statement):
    result = statement["NAME"] + "(" + ",".join(statement["FORMALS"])
    result += ") := " + unparse_actions (statement["ACTIONS"])
    return result + ";\n"

def unparse_command(command, show_actions):
    result = unparse_terms (show_actions, command["TERMS"])
    if command.has_key("ACTIONS") and show_actions: 
        result += " = " + unparse_actions (command["ACTIONS"])
    return result

def unparse_terms(show_actions, terms):
    result = unparse_term(terms[0], show_actions)
    for term in terms[1:]: 
        result += " " + unparse_term(term, show_actions)
    return result

def unparse_term(term, show_actions):
    result = ""
    if term.get("OPTIONAL"): result +=  "["
    
    if   term["TYPE"] == "word":      result += term["TEXT"]
    elif term["TYPE"] == "variable":  result += "<" + term["TEXT"] + ">"
    elif term["TYPE"] == "dictation": result += "<_anything>"
    elif term["TYPE"] == "menu":      
        result += unparse_menu (term, show_actions)
    elif term["TYPE"] == "range": 
        result += str(term["FROM"]) + ".." + str(term["TO"])
    
    if term.get("OPTIONAL"): result +=  "]"
    return result

def unparse_menu(menu, show_actions):
    commands = menu["COMMANDS"]
    result = "(" + unparse_command(commands[0], show_actions)
    for command in commands[1:]: 
        result += " | " + unparse_command(command, show_actions)
    return result + ")"

def unparse_actions(actions):
    if len(actions) == 0: return ""  # bug fix <<<>>>
    result  = unparse_action(actions[0])
    for action in actions[1:]: 
        result += " " + unparse_action(action)
    return result

def unparse_action(action):
    if   action["TYPE"] == "word":      return unparse_word(action)
    elif action["TYPE"] == "reference": return "$" + action["TEXT"]
    elif action["TYPE"] == "formalref": return "$" + action["TEXT"]
    elif action["TYPE"] == "call": 
        result = action["TEXT"] + "("
        arguments = action["ARGUMENTS"]
        if len(arguments) > 0:
            result += unparse_argument(arguments[0])
            for argument in arguments[1:]: 
                result += ", " + unparse_argument(argument)
        return result + ")"
    else:
        return "<UNKNOWN ACTION>"  # should never happen...

def unparse_word(action):
    word = action["TEXT"] 
    word = word.replace("'","''")

    return "'" + word + "'" 

def unparse_argument(argument):
    return unparse_actions(argument)

# ---------------------------------------------------------------------------
# Transform Eval into EvalTemplate, unroll user functions

  # takes a list of non-action nodes
def transform_nodes(nodes):
    for node in nodes: 
        transform_node(node)

def transform_node(node):
    if node.has_key("COMMANDS"):   transform_nodes(node["COMMANDS"]) 
    if node.has_key("TERMS"):      transform_nodes(node["TERMS"]) 
    if node.has_key("MENU"):       transform_node( node["MENU"]) 
    
    if node.has_key("ACTIONS"):    
        substitution = {}
        node["ACTIONS"] = transform_actions(substitution, node["ACTIONS"]) 

# transforms above are destructive, transforms below are functional
# except transform_eval

def transform_actions(substitution, actions):
    new_actions = []
    
    for action in actions: 
        new_actions.extend(transform_action(substitution, action))
    return new_actions

def transform_arguments(substitution, arguments):          # lists of actions
    new_arguments = []
    
    for argument in arguments: 
        new_arguments.append(transform_actions(substitution, argument))
    return new_arguments

def transform_action(substitution, action):  # -> actions
    if action["TYPE"] == "formalref":  
        name = action["TEXT"]
        if substitution.has_key(name):
            return substitution[name]
    if action["TYPE"] == "call":  
        return transform_call(substitution, action)
    return [action] ##= return new List<Node>(){action}

def transform_call(substitution, call):  # -> actions
    global Function_definitions, argument
    new_call = {}
    new_call["TYPE"]      = call["TYPE"]
    new_call["TEXT"]      = call["TEXT"]
    new_call["CALLTYPE"]  = call["CALLTYPE"]
    if call.has_key("ARGTYPES"):  new_call["ARGTYPES"]  = call["ARGTYPES"] 
    new_call["ARGUMENTS"] = call["ARGUMENTS"]
    
    if new_call["CALLTYPE"] == "vocola" and new_call["TEXT"] == "Eval": 
        transform_eval(new_call)
    new_call["ARGUMENTS"] = transform_arguments(substitution, 
                                                new_call["ARGUMENTS"])
    
    if new_call["CALLTYPE"] == "user": 
        arguments  = new_call["ARGUMENTS"]
    
        definition = Function_definitions[new_call["TEXT"]]
        formals    = definition["FORMALS"]
        body       = definition["ACTIONS"]
    
        bindings = {} ##: Dictionary<string,List<Node>>
        i = 0
        for argument in arguments:
            bindings[formals[i]] = argument
            i += 1
        return transform_actions(bindings, body)
    return [new_call] ##= return new List<Node>{new_call}

# Eval() is a special form that takes a single argument, which is
# composed of a series of actions.  A call to EvalTemplate is
# constructed at compile time from the actions where each word action
# supplies a piece of template text and each non-word action denotes a
# hole in the template (represented by "%a") that will be "filled" at
# runtime by the result of evaluating that non-word action.
#
# Example: the template for Eval(1 + $2-$3) is "1+%a-%a", yielding the
# call EvalTemplate("1+%a-%a", $2, $3); assuming $2 has value "3" and
# $3 has value "5", this evaluates to "8".
#
# (Values are treated as integers by %a if and only if they have the
# form of a canonical integer; e.g., 13 but not "013".)

def transform_eval(call):
    arguments = call["ARGUMENTS"]
    
    template = ""
    new_arguments = []
    for action in arguments[0]: 
        if action["TYPE"] == "word": 
            text = action["TEXT"]
            text = text.replace("%", "%%")
            template += text
        else: 
            template += "%a"
            new_argument = []
            new_argument.append(action)
            new_arguments.append(new_argument)
    template_word = {}  ##: Node
    template_word["TYPE"] = "word"
    template_word["TEXT"] = template
    
    template_argument = []
    template_argument.append(template_word)
    new_arguments = [template_argument] + new_arguments ##= new_arguments.Insert(0, template_argument)
    
    call["TEXT"]      = "EvalTemplate" 
    call["ARGUMENTS"] = new_arguments

def get_variable_terms(command):
    variable_terms = []
    for term in command["TERMS"]: 
        type = term["TYPE"]
        if type == "menu" or type == "range" or type == "variable" or type == "dictation": 
            variable_terms.append(term)
    return variable_terms

# ---------------------------------------------------------------------------
# Utilities for transforming command terms into NatLink rules 
#
# For each Vocola command, we define a NatLink rule and an associated
# "gotResults" function. When the command is spoken, we want the gotResults
# function to be called exactly once. But life is difficult -- NatLink calls a
# gotResults function once for each contiguous sequence of spoken words
# specifically present in the associated rule. There are two problems:
#
# 1) If a rule contains only references to other rules, it won't be called 
#
# We solve this by "inlining" variables (replacing a variable term with the
# variable's definition) until the command is "concrete" (all branches contain
# a non-optional word).
#
# 2) If a rule is "split" (e.g. "Kill <n> Words") it will be called twice
#
# We solve this by generating two rules, e.g.
#    <1> exported = 'Kill' <n> <1a> ;
#    <1a> = 'Words' ;

def command_has_a_concrete_term(command):
    for term in command["TERMS"]: 
        if term_is_concrete(term): return True
    return False

def term_is_concrete(term):
    type = term["TYPE"]
    if   type == "menu":                            return True
    elif type == "variable" or type == "dictation": return False
    else: return not term["OPTIONAL"]

# ---------------------------------------------------------------------------
# Utilities used by "emit" methods

def menu_has_actions(menu):
    for command in menu["COMMANDS"]: 
        if command.has_key("ACTIONS"): return True
        for term in command["TERMS"]:
            if term["TYPE"] == "menu" and menu_has_actions(term): return True
    return False

def menu_is_range(menu):  # verified menu => can contain only 1 range as a 1st term
    commands = menu["COMMANDS"]
    for command in commands: 
        terms = command["TERMS"]
        type = terms[0]["TYPE"]
        if type == "menu" and menu_is_range(terms[0]):  return True 
        if type == "range":  return True
    return False

# To emit actions for a menu, build a flat list of (canonicalized) commands:
#     - recursively extract commands from nested menus
#     - distribute actions, i.e. (day|days)=d --> (day=d|days=d)
# Note that error checking happened during parsing, in verify_referenced_menu

def make_safe_python_string(text):
    text = text.replace("\\", "\\\\")
    text = text.replace("'", "\\'")
    text = text.replace("\n", "\\n")

    return text

# ---------------------------------------------------------------------------
# Okay, let's run!

#main();

##################################################


## 
## Tokenizer:
## 

Singles      = r'()\[\],|;='

  # characters not allowed anywhere in a bare word:
Excluded     = r'\s\#\'\"' + Singles

Pseudo_token = r'(?x) '                                                  + \
               r' \s* (?: \#.*\n \s* )* '                                + \
               r' ( [^:'+Excluded+r']+ (?: :+ [^:'+Excluded+r']+ )* '    + \
               r' | ['+Singles+r'] '                                     + \
               r' | \" [^\"\n]* (?: \"\" [^\"\n]* )* [\"\n] '            + \
               r' | \' [^\'\n]* (?: \'\' [^\'\n]* )* [\'\n] '            + \
               r' | := '                                                 + \
               r' | :+ [^:'+Excluded+r']+ (?: :+ [^:'+Excluded+r']+ )* ' + \
               r' | : '                                                  + \
               r' )'

Pseudo       = re.compile(Pseudo_token)


Token_properties = {}   ##: Dictionary<string,int>

def initialize_tokenizer(token_properties):
    global Token_properties

    properties = token_properties
    for i in xrange(0,256): ##=
    ##= i = 0
    ##= while i < 256:
        c = chr(i) ##= c = Char.ConvertFromUtf32(i) ##: string
        token_properties[c + ':']  = token_properties[':']
        token_properties[c + '"']  = token_properties['"']
        token_properties[c + "'"]  = token_properties["'"]
        token_properties[c + "\n"] = token_properties["\n"]
        ##= i += 1

    Token_properties = properties

  # requires: text ends in a newline
def tokenize(text):  # -> [[kind, token text, offset in text of token start]*]
    global Token_properties, Pseudo
    properties               = Token_properties
    token_bare_properties    = properties["b"]
    token_context_properties = properties[":"]
    pseudo                   = Pseudo

    tokens          = []  ##: List<List<dynamic>>

    start           = 0
    statement_start = 0
    while True:
        match = pseudo.match(text, start)
        if not match:
            tokens.append([properties["EOF"], "", len(text)]) ##= tokens.append(new List<dynamic>{properties["EOF"], "", len(text)})
            return tokens

        start = match.end(0)
        token = match.group(1)
        kind  = properties.get(token[-2:], token_bare_properties) ##=
        ##= key = token[-2:]
        ##= if properties.has_key(key):
            ##= kind  = properties[key]
        ##= else: kind = token_bare_properties
        tokens.append([kind, token, match.start(1)])

        if token == ";":
            statement_start = len(tokens)
        elif kind == token_context_properties:
            beginning = tokens[statement_start][2]
            token     = text[beginning:start]
            tokens[statement_start:] = [[kind, token, beginning]] ##=
            ##= tokens.RemoveRange(statement_start, tokens.Count() - statement_start)
            ##= tokens.Add([kind, token, beginning])
            statement_start = len(tokens)

# 
# Every string ending with a newline can be divided into a continuous 
# series of these, with each pseudo token being as greedy as possible.
# 



## 
## 
## 

TOKEN_BARE_WORD    = 0x1
TOKEN_DOUBLE_WORD  = 0x2
TOKEN_SINGLE_WORD  = 0x4
TOKEN_LPAREN       = 0x8
TOKEN_RPAREN       = 0x10
TOKEN_LBRACKET     = 0x20
TOKEN_RBRACKET     = 0x40
TOKEN_BAR          = 0x80
TOKEN_COMMA        = 0x100
TOKEN_SEMICOLON    = 0x200
TOKEN_EQUALS       = 0x400
TOKEN_COLON_EQUALS = 0x800
TOKEN_CONTEXT      = 0x1000
TOKEN_EOF          = 0x2000

TOKEN_TERM         = 0x10000
TOKEN_ACTION       = 0x20000


def initialize_token_properties():
    properties = {}  ##: Dictionary<string,int>

    properties["("]   = TOKEN_LPAREN|TOKEN_TERM
    properties[")"]   = TOKEN_RPAREN

    properties["["]   = TOKEN_LBRACKET|TOKEN_TERM
    properties["]"]   = TOKEN_RBRACKET

    properties["|"]   = TOKEN_BAR
    properties[","]   = TOKEN_COMMA
    properties[";"]   = TOKEN_SEMICOLON
    properties["="]   = TOKEN_EQUALS
    properties[":="]  = TOKEN_COLON_EQUALS

    properties[":"]   = TOKEN_CONTEXT
    properties["b"]   = TOKEN_BARE_WORD|TOKEN_TERM
    properties['"']   = TOKEN_DOUBLE_WORD|TOKEN_TERM
    properties["'"]   = TOKEN_SINGLE_WORD|TOKEN_TERM
    properties["\n"]  = TOKEN_SINGLE_WORD|TOKEN_TERM # <<<>>>
    properties["EOF"] = TOKEN_EOF

    initialize_tokenizer(properties)

Tokens = []  ##: List<List<dynamic>>
Peeks = 0
Offset = 0
Text = ""   ##: string
Scan_limit = 0
Scan_newlines = 0

def load_tokens():
    global Text, Offset, Peeks, Tokens, Scan_limit, Scan_newlines

    Offset = 0
    Tokens = tokenize(Text)
    Peeks  = 0
    Scan_limit = 0
    Scan_newlines = 0



def tokenizer_status():
    global Text, Tokens, Offset, Peeks, Scan_limit, Scan_newlines
    return [Text, Tokens, Offset, Peeks, Scan_limit, Scan_newlines]

def restore_status(status):
    global Text, Tokens, Offset, Peeks, Scan_limit, Scan_newlines
    Text, Tokens, Offset, Peeks, Scan_limit, Scan_newlines = status



    



Text_stack = []  ##: List<List<dynamic>>

def open_text(text):
    global Text_stack, Text

    Text_stack.append(tokenizer_status())

    if text == "" or text[-1]!="\n":  ##=     if text == "" or text.Substring(text.Length-1,1)!='\n':
        text += "\n"

    Text = text
    load_tokens()

def close_text():
    global Text_stack
    status = Text_stack.pop()
    restore_status(status)



def get_current_position():
    global Offset, Tokens, Text
    return [Offset, Tokens, Text]

def rewind(position):
    global Offset, Tokens, Peeks
    Offset, Tokens, text = position
    Peeks = 0


def get_line_number(position): ##: int
    global Text, Scan_limit, Scan_newlines

    offset, tokens, text = position
    text_offset = tokens[offset][2]

    if text is Text: ##= if object.ReferenceEquals(text, Text):
        if text_offset < Scan_limit:
            Scan_limit = Scan_newlines = 0
        Scan_newlines += text[Scan_limit:text_offset].count("\n")
        Scan_limit = text_offset
        return Scan_newlines + 1

    return text[:text_offset].count("\n")+ 1




def decode_token_kinds(kind):
    result = []  ##: List<string>
    if kind & TOKEN_ACTION:
        result.append("an action")
    if kind & TOKEN_TERM:
        result.append("a term")

    if kind & TOKEN_LPAREN:
        result.append("'('")
    if kind & TOKEN_RPAREN:
        result.append("')'")

    if kind & TOKEN_LBRACKET:
        result.append("'['")
    if kind & TOKEN_RBRACKET:
        result.append("']'")

    if kind & TOKEN_BAR:
        result.append("'|'")
    if kind & TOKEN_COMMA:
        result.append("','")
    if kind & TOKEN_SEMICOLON:
        result.append("';'")
    if kind & TOKEN_EQUALS:
        result.append("'='")
    if kind & TOKEN_COLON_EQUALS:
        result.append("':='")

    if kind & TOKEN_CONTEXT:
        result.append("a context statement")

    if kind & TOKEN_BARE_WORD:
        result.append("a bare word")
    if kind & TOKEN_DOUBLE_WORD:
        result.append("a double quoted word")
    if kind & TOKEN_SINGLE_WORD:
        result.append("a single quoted word")

    return ", ".join(result)











def eat(kind):   # <<<>>> ##: List<dynamic>
    global Tokens, Offset, Peeks
    if Tokens[Offset][0] & (TOKEN_DOUBLE_WORD|TOKEN_SINGLE_WORD):
        if Tokens[Offset][1][-1] == "\n":
            raise SyntaxError, "Unterminated quotation: " + Tokens[Offset][1][:-1]
    if not (Tokens[Offset][0] & kind):
        Peeks |= kind
        raise SyntaxError, "wanted " + decode_token_kinds(Peeks) + " but found: "+ repr(Tokens[Offset]) ##=
        ##= raise SyntaxError, "wanted " + decode_token_kinds(Peeks) + " but found: ..."
    if Tokens[Offset][0] & TOKEN_EOF:
        raise RuntimeError, "fail" # <<<>>>
    Offset += 1
    Peeks = 0
    return Tokens[Offset -1]

def peek(kind):
    global Tokens, Offset, Peeks
    Peeks |= kind
    return Tokens[Offset][0] & kind

    #log_error("Unterminated quotation: " + $5 + "" + $6)

def eat_any():
    global Tokens, Offset, Peeks
    eat(Tokens[Offset][0])


import sys

Input_name = ""
Variable_terms = []
Formals        = []
Functions      = {}   ##: Dictionary<string,int>
Include_stack  = []   ##: List<dynamic>
Definitions    = {}   ##: Dictionary<string,Node>
Forward_references = []
Function_definitions = {}   ##: Dictionary<string,Node>
Included_files = []   # <<<>>> ##: List<string>
Statement_count = 1
Error_count = 0
Debug = 0
LOG = open("source/vcl2py_log.txt", "w")  ##= LOG = new StreamWriter(File.OpenWrite("source/vcl2py_log.txt"))  ##: StreamWriter
default_maximum_commands = 1
VocolaVersion = "2.7.2"
Number_words = {}                             ##: Dictionary<int,string>
NestedCallLevel = 0

def die(error): ##: Exception
    raise RuntimeError, error    # <<<>>>

    


def main():
    print "hello world!"

    read_extensions_file("/home/mdl/Tmp/working/extensions.csv")
    #print Extension_functions

    initialize_token_properties()

    for file in sys.argv[1:]:
        file = file.replace(".vcl", "")
        file = file.replace("source/", "")
        convert_file(file, "target", "_vcl")



def main2():
    print "hello world!"

    test = 'hello world = "hello world!";'

    initialize_token_properties()
    open_text(test)

    print unparse_command(parse_statement(), True)

main()
#import profile
#profile.run('main()')
