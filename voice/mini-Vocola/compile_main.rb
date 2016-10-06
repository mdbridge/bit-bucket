require './statements.rb'

class Compile

  def main
    Statements.new(ARGV[0]).each do |statement|
      if statement.tokens[-1] != ";"
        statement.error "context statements not yet implemented"
        next
      elsif (directive = statement.directive?)
        statement.error "unknown directive '#{directive}'"
        next
      end

      tokens = statement.tokens.dup
      tokens.pop
      
      if tokens.include?("=")
        compile_macro(tokens)
      elsif tokens.include?(":=")
        compile_function(tokens)
      else
        statement.error "unknown statement type"
      end
    end
  end

  def compile_macro(tokens)
    @tokens = tokens

    words = []
    lists = 0
    while (word = @tokens.shift) != "="
      word = Tokens.unquote(word)
      word.sub!(/^(\d\d*)\.\.(\d\d*)$/, '<\1to\2>')
      lists += 1 if word.sub!(/^<(.*)>$/, '[mv-\1]')
      words << word
    end

    command = words.join(" ")
    print_header(command)
    1.upto(lists) { |n| puts <<"DEFINITION"
    arg#{n} = Replace$(Replace$(Left$(ListVar#{n},InStr(ListVar#{n}+"\\","\\")-1),"&B","\\"),"&A","&")
DEFINITION
    }
    puts if lists>0

    start_actions
    send_dragon_keys(get_mono_expresssion) while @tokens.length > 0
    end_actions

    print_trailer
  end


  def print_header(command_name)
    #$stderr.puts "    #{command_name}"
#    puts
    puts <<"HEADER"
<Command name="#{command_name}" group="automatically generated" enabled="true" states="">
<description>AUTOMATICALLY GENERATED MACRO; DO NOT EDIT!</description>
<contents type="SCRIPT">
<![CDATA[Sub Main
HEADER
  end

  def print_trailer
    puts "End Sub\n"

    @includes.each do |include|
      puts "\n' ============== included file #{include} ==============\n\n"

      File.open(include) { |x| puts(x.readlines()) }
   end

    puts <<TRAILER
]]>
</contents>
</Command>
TRAILER
  end


  def start_actions
    @previous = nil                  # no unflushed output exists 
    @includes = []
  end

  def flush
    #puts @previous if @previous
    puts @previous + ", \"_\", \" \")" if @previous
    @previous = nil
  end

  def end_actions
    flush
  end

  def send_dragon_keys(expression)
    return if expression == nil

    puts @previous + " _" if @previous
    if @previous
      @previous = "                 & #{expression}"
    else
      #@previous = "    SendDragonKeys #{expression}"
      @previous = "    SendDragonKeys Replace(#{expression}"
    end
  end

  def command(statement)
    flush
    puts statement
  end



  def get_mono_expresssion
    token = @tokens.shift

    if @tokens.first != "("
      # keysequence:
      keys = token
      keys = '"' + keys + '"' if keys !~ /"/

      keys.gsub!(/\\\$/, "\n")
      keys.gsub!(/\$([0-9][0-9]*)/, '" & arg\1 & "')
      keys.gsub!(/\$([a-zA-Z][a-zA-Z0-9_]*)/, '" & mv_\1 & "')
      keys.gsub!(/\n/, "$")

      keys.sub!(/^"" & /, "")
      keys.sub!(/ & ""$/, "")

      return keys
    end

    # routine_name '(' expressions ')':
    @tokens.shift   # eat '('
    arguments = get_expressions
    @tokens.shift   # eat ')'

    return compile_routine(token, arguments)
  end

  # mono+:
  def get_expression
    result = get_mono_expresssion

    peek = @tokens.first
    return result if peek == nil || peek == ")" || peek == ","

    return get_expression if !result

    rest = get_expression
    return result if !rest

    return result + " & " + rest
  end

  def get_expressions
    # empty:
    return [] if @tokens.first == ")"

    # expression [',' expression]*:
    result = [get_expression]
    while @tokens.first == ","
      @tokens.shift   # eat ','
      result.push(get_expression)
    end

    return result
  end



  def compile_routine(name, arguments)
    lookup = @routines[name]
    if lookup
      return lookup.call(name, arguments)
    end

    $stderr.puts "***** Unknown routine: #{name}"
    command("    MsgBox \"Unknown mini-vocola routine: #{name}\"")
    command("    Exit Sub")
    return '""'
  end



  def returns_int(closure)
    proc { |name,args|
             "CStr(#{closure.call(name, args)})"
         }
  end
    
  def int(closure, arg_index)
    proc { |name,args|
	     arg = args[arg_index]
             if arg then
               args[arg_index] =
	         case arg
		   when /^"(\d+)"$/ then $1
	           else "Val(#{arg})"
		 end
             end
             closure.call(name, args)
         }
  end

  def function2(real_name)
    proc { |name,args| 
             args = args.join(", ")
             "#{real_name}(#{args})" 
         }
  end


  def initialize
    function =  proc { |name,args| 
                         args = args.join(", ")
                         "#{name}(#{args})" 
		     }
    procedure = proc { |name,args| 
                         args = args.join(", ")
		         command("    #{name} #{args}")
			 nil
                     }

    @routines = {
           # This doesn't work for actions with side effects:
        "Repeat"		=> 
             int(proc { |name,args|
                           "Replace$(Space$(#{args[0]}),\" \", #{args[1]})"
                      }, 0),

        "Wait"			=> 
             int(proc { |name,args|
                           flush
	                   command("    Wait #{args[0]}/1000.0")
			   nil
                      }, 0),

        "AppBringUp"       => procedure,
        "Beep"             => procedure,
        "ButtonClick"      => int(int(procedure, 0), 1),
        "DdeExecute"       => int(int(procedure, 0), 2),
        "HeardWord"        => procedure,
        "MouseGrid"        => int(int(procedure, 0), 1),
        "MsgBoxConfirm"    => int(procedure, 1),
        "SendKeys"         => procedure,
        "SendDragonKeys"   => procedure,
        "SendSystemKeys"   => procedure,
        "SetMousePosition" => int(int(int(procedure, 0), 1), 2),
        "ShellExecute"     => int(procedure, 1),
        "Shell"            => procedure,
        "ShiftKey"         => int(int(procedure, 0), 1),
        #"WaitForWindow"  NOT AVAILABLE IN DNS ANY MORE

        "LCase"            => function,
        "Left"             => int(function, 1),
        "Len"              => returns_int(function),
        "Replace"          => function,
        "Right"            => int(function, 1),
        "UCase"            => function,

        "Clipboard.Get" => function2("Clipboard"),

	"First"	            	=>
             proc { |name,args|
              "Left(#{args[0]}, InStr(#{args[0]} & #{args[1]}, #{args[1]})-1)"
                  },
	"Second"		=>
             proc { |name,args|
              "Mid$(#{args[0]}, InStr(#{args[0]} & #{args[1]}, #{args[1]})+1)"
                  },

	"Last"		=>
             proc { |name,args|
              "Mid$(#{args[0]}, InStrRev(#{args[1]}&#{args[0]}, #{args[1]}))"
                  },

        "Special.EQ" => 
             proc { |name,args| 
               "IIf(#{args[0]} = #{args[1]}, \"1\", \"0\")"
             },
        "Special.Split" => 
             proc { |name,args| 
               file = "extensions/split.vb"
               @includes.push(file) if !@includes.include?(file)
               args = args.join(", ")
               "Split(#{args})" 
             },

        "INCLUDE"               =>
             proc { |name,args| 
               file = Tokens.unquote(args[0])
               @includes.push(file) if !@includes.include?(file)
               nil
             },

        "PROC"               =>
             proc { |name,args| 
              function = Tokens.unquote(args.shift)
              command("    If Not #{function}(#{args.join(', ')}) Then\n" +
                      "        Exit Sub\n" +
                      "    End If")
             },

	"Eval"		=>
             proc { |name,args|
              "Eval(#{args[0]})"
                  }

    }
  end


  def compile_function tokens
    name = tokens.shift

    tokens.shift  # eat "("
    formals = []
    while tokens[0] != ")"
      formals << tokens.shift
      tokens.shift if tokens[0] == ","
    end
    tokens.shift  # eat ")"

    tokens.shift  # eat ":="
    definition = tokens.clone

    @routines[name] = lambda do |name2,args|
      body = definition.clone
      if args.length != formals.length then
        $stderr.puts "***** Incorrect number of arguments in call to #{name}"
        command("    MsgBox \"mini-vocola routine: #{name} called with the wrong number of arguments\"")
        command("    Exit Sub")
        body = '""'
      else
        0.upto(formals.length-1) do |i|
          #$stderr.puts "#{args[i]} for #{formals[i]}"
          body = body.collect do |token|
            token=="$#{formals[i]}" ? '"" & ' + args[i] + ' & ""' : token
          end
        end
      end

      @tokens = body + @tokens
      nil
    end
  end
end
 
Compile.new.main
