##
## Setup:
##

input = "unparse.py"
input = ARGV[0] if ARGV.length>0

$program = File.read(input)
fail if $program =~ /\t/



## 
## Deal with DOS encoding:
## 

$program.gsub!(/\r/, "") 
def out(n)
  File.open("out#{n}.cs", "w") do |output|
    program = $program
    program = program.gsub(/\r/, "!")
    program = program.gsub(/\f/, "^")

    program = program.gsub(/\r/, "\n")
    program = program.gsub(/\f/, "\n")
    output.print program.gsub(/\n/, "\r\n")
  end
end

out(0)



## 
## 
## 

$program.gsub!(/^([ ]*).*?\#\#= *(.*)$/, "\\1\\2")

$program.gsub!(/if kind \&(.*?):/, "if 0 != (kind &\\1):")



## 
## Fold up multiple parts of a single logical line into one physical line:
## 

#
# Hide contents of multiline quotations:
#

$program.gsub!(/(\"\"\".*?\"\"\") | (\'\'\'.*?\'\'\')/xm) do |quotation|
  fail if quotation[3..-4] =~ /[#{quotation[0]}\\]/
  quotation.gsub(/\n/, "\r")
end

out(1)


  # This does not recognize entire quotations per se, but rather what
  # they escape now:
Quoted = /(?: \" (?:  [^\"\\] | \\. )* \" |
              \' (?:  [^\'\\] | \\. )* \' )/x


#
# Fold up explicit continuation lines:
#

$program.gsub!(/^( (?: #{Quoted} | [^\"\'\n\#] )* )  \\ \n/x, "\\1\f")

out(2)


# 
# Fold up implicit continuation lines:
# 

Space      = /[\040]/  # 1 space character

Comment    = /(?: \# [^\n]* $ )/x


Balanced_0 = / (?: [^{()}\[\]\"\'\n\#] | #{Quoted} ) /x

Balanced_1 = / (?: #{Balanced_0} 
                 | \[ (?: #{Balanced_0}|#{Comment}?\n )* \]
                 | \( (?: #{Balanced_0}|#{Comment}?\n )* \) 
                 | \{ (?: #{Balanced_0}|#{Comment}?\n )* \} ) /x

Balanced_2 = / (?: #{Balanced_1} 
                 | \[ (?: #{Balanced_1}|#{Comment}?\n )* \]
                 | \( (?: #{Balanced_1}|#{Comment}?\n )* \) 
                 | \{ (?: #{Balanced_1}|#{Comment}?\n )* \} ) /x

$program.gsub!(/^(#{Balanced_2}+)/x) { |x| x.gsub(/\n/, "\f") }

out(3)


# WARNING: embedded comments can now appear within lines



## 
## Remove unneeded lines:
## 

Embedded_comment = /(?: \# [^\n\f]* \f )/x

  # below intentionally does not allow embedded comments:
Simple_code = /(?: [^\#\"\'\n] | #{Quoted} )/x

$program.gsub!(/^( *)(global|import|from)\b#{Simple_code}+(\#[^\n]*)?\n/) do
  # preserve comment on line if any:
  if $3 then "#$1#$3\n" else "" end
end

out(10)



## 
## Put semicolons in:

## 

White    = /(?: [\040\f] | #{Embedded_comment} )/x

  # a bit of code, never white space or empty:
Code_bit = /(?: [^\#\"\'\s] | #{Quoted} )/x

  # nonempty code, starting and ending with non-whitespace:
Code     = /(?: #{Code_bit} (?: #{White}* #{Code_bit})* )/x


$program.gsub!(/^( #{White}* #{Code} ) (?<! :)
                 ( #{White}* (?:\#[^\n]*)? )$/x, "\\1;\\2")

out(11)



## 
## Fixup control structures:
## 

Empty_line = /(?: #{Space}* (?:\#.*)? \n ) /x

1.upto(10) do
  $program.gsub!(/^(#{Space}*)(if|elif|else|while|try|except|finally|for|def)\b
                 (#{Space}*)(#{Balanced_2}*):
                 (?: (#{Space}*) (#{Code}) ( #{Space}* (?:\#.*)? ) $ 
                   | ( #{Space}* (?:\#.*)? \n
                       (?: #{Empty_line}* \1#{Space} .*\n )+ )
                 )/x) do 
    white_1, keyword, white_2, argument, white_3, single, white_4, multi = 
      $1, $2, $3, $4, $5, $6, $7, $8

    argument.gsub!(/,/, "")              if keyword == "except"
    argument = "var "+argument           if keyword == "for"
    argument = "(#{argument})"           if argument!="" and keyword != "def"
    argument.gsub!(/\[1:\]/, ".Skip(1)") if keyword == "for"

    keyword = "foreach"                  if keyword == "for"
    keyword = "catch"                    if keyword == "except"
    keyword = "else if"                  if keyword == "elif"
    keyword = "public static"            if keyword == "def"

    start = white_1 + keyword + white_2 + argument
    if single then
      "#{start}#{white_3}{ #{single} } #{white_4}"
    else
      "#{start} {#{multi}#{white_1}}\n"
    end
  end
end


$program.gsub!(/^(#{Space}*) }\n (?:#{Space}*\n)* \1 (else|catch|finally)/x) { |x| "#$1} #$2" }

out(20)



## 
## Convert common operators:
## 

# replace continuous pieces of code either outside quotes and comments
# or quotes themselves; pieces do not cross newlines
def gsub_selected(text, in_quotes=false)
  text.gsub(/^.*$/) do |line|
    line.gsub(/\G( \"\"\".*?\"\"\"
                 | \'\'\'.*?\'\'\'
                 | r?\"([^\"\\]|\\.)*\" 
                 | r?\'([^\'\\]|\\.)*\'
                 | ([^\"\'\#r] | r(?![\"\']))+
                 )/x) do |piece|
      if (piece =~ /^r?[\'\"]/ and in_quotes) or 
         (piece !~ /^r?[\'\"]/ and not in_quotes) then
        yield piece
      else
        piece
      end
    end
  end
end


$program.gsub!(/\[\"([A-Z]+(?<!EOF))\"\]/, ".\\1")

  # default value of OPTIONAL is False:
$program.gsub!(/\.get\(\"OPTIONAL\"(, *False)?\)/, ".OPTIONAL")

out(30)


$program = gsub_selected($program, false) do |code|
  code.gsub(/\s+or\s+/, " || ").
    gsub(/\s+and\s+/, " && ").
    gsub(/\bnot\s+/, "!").
    gsub(/\bTrue\b/, "true").
    gsub(/\bFalse\b/, "false").
    gsub(/\bNone\b/, "null").
    gsub(/\bIOError\b/, "IOException").
    gsub(/\bRuntimeError\b/, "SystemException").
    gsub(/int\(/, "Int32.Parse(").
    gsub(/\.replace\(/, ".Replace(").
    gsub(/\.append\(/, ".Add(").
    gsub(/\.extend\(/, ".AddRange(").
    gsub(/\.has_key\(/, ".ContainsKey(").
    gsub(/\bstr\(([^\'\"\#\n)]*)\)/, "\\1.ToString()").
    gsub(/\blen\(([^\'\"\#\n)]*)\)/, "\\1.Count()").
    gsub(/\[([^\],]*):([^\],]*)\]/) {
       ".Slice(" + ($1!="" ? $1 : "0") + "," + ($2!="" ? $2 : "-1") + ")"
    }.
    gsub(/\[([^\]:,]*),([^\]:]*)\]/, "new List<dynamic>{\\1,\\2}")
end

$program.gsub!(/(\"[^"]*\")\.join\(/, "String.Join(\\1, ")

$program.gsub!(/\[\"\"\]/, 'new List<string>{""}')

out(31)


$program.gsub!(/^(.*)\bprint >>(\w+), (.*?)(,?);/) {
  "#$1#$2.Write" + ($4!=',' ? "Line" : "") + "(#$3);"
}

$program.gsub!(/^(.*)\bprint (.*?)(,?);/) {
  "#$1Console.Write" + ($3!=',' ? "Line" : "") + "(#$2);"
}

out(32)


$program.gsub!(/^([ ]*)(\w+(,\s*\w+)+)\s*=\s*(.*);$/) do |unchanged|
  white_1, variable_list, value = $1, $2, $4
  variables = variable_list.split(/,\s*/)

  output = ""
  variables.each_with_index {|v,i| output += "#{white_1}#{v} = #{value}[#{i}];\n" }
  output
end

out(33)


$program.gsub!(/re\.compile\((#{Balanced_2}+)\)/,
               "new Regex(\\1)")


$program.gsub!(/re\.split\((#{Balanced_2}+),\s*(#{Balanced_2}+)\)/,
               "new List<string>(Regex.Split(\\2, \\1))")

$program.gsub!(/re\.sub\((#{Balanced_2}+),\s*(#{Balanced_2}+),\s*(#{Balanced_2}+)\)/,
               "Regex.Replace(\\3, \\1, \\2)")


$program.gsub!(/!re\.match\((#{Balanced_2}+),\s*(#{Balanced_2}+)\)/,
               "!Regex.IsMatch(\\2, \\1)")

$program.gsub!(/re\.match\((#{Balanced_2}+),\s*(#{Balanced_2}+)\)/,
               "Regex.Match(\\2, \\1)")


$program.gsub!(/re\.search\((#{Balanced_2}+),\s*(#{Balanced_2}+)\)/,
               "Regex.Match(\\2, \\1)")


out(34)


$program.gsub!(/(^[ ]*|{ )die\((#{Balanced_2}+)\)/, '\\1die(\\2); throw new SystemException("fail")')

$program.gsub!(/(^[ ]*|{ )raise (\w+), (.*);/, "\\1throw new \\2(\\3);")

out(35)



## 
## Add types:
## 

$globals = {}

def infer_type(name, line)
  if line =~ /\#\#: ([\w<>,\[\]]*)\s*$/ then
    return $1
  end

  return $globals[name] if $globals[name]


  singles = {"argument" => "List<Node>", "formal" => "string"}

  node_names = %w{node statement context directive command action menu term reference call}
  node_names.each do |node_name|
    singles[node_name] = "Node"
  end

  map = {"substitution" => "Dictionary<string,List<Node>>",
         "separators" => "int", "kind" => "int", "offset" => "int",
         "has" => "bool"}
  singles.each_key do |noun|
    map[noun] = singles[noun]
    map[noun+'s'] = "List<#{singles[noun]}>"
  end


  if name == "show_actions" then
    return "bool"
  end

  case line
  when /=\s*true/i then return "bool"
  when /=\s*false/i then return "bool"
  when /=\s*\d/i then return "int"
  end

  map.each do |pattern, type|
    return type if name =~ /(\b|_)#{pattern}(\b|_)/i
  end

  #return "string"
  return "dynamic"
end


def infer_return_type(name, body)
  if body =~ /\A[^\n]*\#\#: ([\w<>,\[\]]*)\s*$/ then
    return $1
  end

  return "void" unless body =~ /\breturn\b[^;\n]/

  if name == "Main" then
    return_type = "int"
  elsif name == "peek" then
    return_type = "bool"    # <<<>>>
  elsif name =~ /^unparse_/ then
    return_type = "string"
  elsif name =~ /^parse_(\w+)$/ then
    return_type = infer_type($1, "")
    return_type = "Node" if return_type == "string"
    return_type = "List<Node>" if name == "parse_file"
  elsif name =~ /^is/ then
    return_type = "bool"
  elsif name =~ /_node$/ then
    return_type = "Node"
  else
    #    return_type = "string"
    return_type = "dynamic"
  end

  return return_type
end


def alter_value(value, type)
  value.gsub(/\[\]|{}/, "new #{type}()")
end



$program.gsub!(/(^public static .*\n([^}].*\n)*}\n)|(^ *\w+ *=.*)/) do |unchanged|
  if unchanged =~ /^public/ then
    unchanged
  else
    fail unless unchanged =~ /(^ *)(\w+)(.*)/m
    white_1, name, value = $1, $2, $3
    type = infer_type(name, unchanged)
    $globals[name] = type
    value = alter_value(value, type)
    "public static #{white_1}#{type} #{name}#{value}"
  end
end


out(50)



def process_routine(routine)
  fail unless routine =~ /^public static (\w+)\((.*?)\) {(.*)/m
  name, argument_list, body = $1, $2, $3
  arguments = argument_list.split(/,\s*/)

  local_types = $globals.dup
  arguments = arguments.collect do |argument|
    fail unless argument =~ /^(\w+)(.*)/
    type = infer_type($1, argument)
    local_types[$1] = type
    "#{type} #$1#{alter_value($2,type)}"
  end

  return_type = infer_return_type(name, body)
  #puts "#{name}-> #{return_type}"

  declarations = ""
  body.gsub!(/^( *)(\w+)( *)= (.*)/) do |unchanged|
    white_1, variable, white_2, value = $1, $2, $3, $4

    if local_types[variable] then
      type = local_types[variable]
      unchanged = alter_value(unchanged, type)
    else
      type = infer_type(variable, unchanged)
      local_types[variable] = type
      value = alter_value(value, type)

      if white_1 != "    " then
        declarations += "    #{type} #{variable};\n"
        type = ""
      else
        type = "var"
      end
      "#{white_1}#{type} #{variable}#{white_2}= #{value}"
    end
  end
  body.sub!(/\n/, "\n" + declarations)

  "public static #{return_type} #{name}(#{arguments.join(', ')}) {#{body}"
end

$program.gsub!(/^public static [^=(]*\(.*\n([^}].*\n)*}\n/) { |x| process_routine(x) }

out(51)



## 
## Fix string literals and comments simultaneously:
## 

def gsub_quotes_and_comments(text)
  text.gsub(/^.*$/) do |line|
    line.gsub(/\G( \"\"\".*?\"\"\"
                 | \'\'\'.*?\'\'\'
                 | r?\"([^\"\\]|\\.)*\" 
                 | r?\'([^\'\\]|\\.)*\'
                 | ([^\"\'\#r] | r(?![\"\']))+
                 | \# [^\f\n]* (?: \f|$ )
                 )/x) do |piece|
      if piece =~ /^r?[\'\"\#]/ then
        yield piece
      else
        piece
      end
    end
  end
end

$program = gsub_quotes_and_comments($program) do |quotes|
  if quotes =~ /^\#(.*)/ then
    "//#$1"
  elsif quotes =~ /^\"\"\".*\"\"\"$/ then
    '@"' + quotes[3..-4].gsub('"', '""') + '"'
  elsif quotes =~ /^\'\'\'.*\'\'\'$/ then
    '@"' + quotes[3..-4].gsub('"', '""') + '"'
  elsif quotes =~ /^r\"/ then
    "@" + quotes[1..-1]
  elsif quotes =~ /^r\'/ then
    '@"' + quotes[2..-2].gsub('"', '""') + '"'
  elsif quotes =~ /^\'/ then
    '"' + quotes[1..-2].gsub('"', '\\"') + '"'
  else
    quotes
  end
end

out(80)



## 
## Unfold logical lines, indenting entire program by 4 except for
## multi-line comments:
## 

$program.gsub!(/\f/, "\n")
$program.gsub!(/^/, "    ")
$program.gsub!(/\r/, "\n")

out(81)



## 
## Wrap with outer class:
## 

$program = <<HEADER + $program + <<TRAILER
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;                 // for Skip
using System.Text.RegularExpressions;


public static class Extensions {
    public static string Slice(this string s, int x, int y) {
        return "XXX";
    }

    public static Node pop<T>(this List<T> node) {
        return new Node();
    }
}


public class Compiler {

HEADER

    static int Main() {
        main();
	return 0;
    }
}


public class Node {
    // Sentinel values:
    private static string sentinel_string = "Xyzzy".Replace("X","x");
    private static Node sentinel_node = new Node();
    private static List<Node> sentinel_nodes = new List<Node>();


    // statement:
    public string TYPE;

    // command:
    public string NAME;
    public List<Node> TERMS = sentinel_nodes;
    public List<Node> ACTIONS = sentinel_nodes;
    public int LINE;
    public string FILE;

    // definition:
    public Node MENU = sentinel_node;

    // function:
    public List<string> FORMALS;

    // context:
    public List<string> STRINGS;
    public List<string> RULENAMES;

    // include:
    public string TEXT;

    // set:
    public string KEY;


    // term:
    public int NUMBER;

    // word:
    public bool OPTIONAL = false;

    // range:
    public int FROM;
    public int TO;

    // menu:
    public List<Node> COMMANDS = sentinel_nodes;

    // call:
    public string CALLTYPE;
    public string ARGTYPES = sentinel_string;
    public List<List<Node>> ARGUMENTS;


    // for formal_reference's only:  (not a parsing node) 
    public string VARIABLE;
    public string MESSAGE;


    public bool ContainsKey(string field) {
        switch (field) {
        case "ACTIONS":
            return !object.ReferenceEquals(ACTIONS, sentinel_nodes);
        case "COMMANDS":
            return !object.ReferenceEquals(COMMANDS, sentinel_nodes);
        case "TERMS":
            return !object.ReferenceEquals(TERMS, sentinel_nodes);

        case "MENU":
            return !object.ReferenceEquals(MENU, sentinel_node);

        case "ARGTYPES":
            return !object.ReferenceEquals(ARGTYPES, sentinel_string);

        default:
            throw new ArgumentException();
        }
    }
}


public class SyntaxError : System.ApplicationException
{
    public SyntaxError() {}
    public SyntaxError(string message) {}
    public SyntaxError(string message, System.Exception inner) {}
 
    // Constructor needed for serialization 
    // when exception propagates from a remoting server to the client.
    protected SyntaxError(System.Runtime.Serialization.SerializationInfo info,
        System.Runtime.Serialization.StreamingContext context) {}
}
TRAILER

out(99)
