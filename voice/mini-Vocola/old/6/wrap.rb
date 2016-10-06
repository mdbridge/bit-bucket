#!/home/mdl/Ruby/bin/ruby -w

class Compile

  def main
    while line = gets
      compile_statement(line.chomp.split(/\t/))
    end
  end

  def compile_statement tokens
    if tokens.include?("=")
      compile_macro(tokens)
    elsif tokens.include?(":=")
      compile_function(tokens)
    else
      $stderr.puts "***** Bad statement: #{tokens.join(' ')}"
    end
  end

  def compile_macro(tokens)
    @tokens = tokens

    words = []
    lists = 0
    while (word = @tokens.shift) != "="
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
#    puts
    puts <<"HEADER"
<Command name="#{command_name}" group="automatically generated" enabled="true" states="">
<description>AUTOMATICALLY GENERATED MACRO; DO NOT EDIT!</description>
<contents type="SCRIPT">
<![CDATA[Sub Main
HEADER
  end

  def print_trailer
    puts <<TRAILER
End Sub\n]]>
</contents>
</Command>
TRAILER
  end


  def start_actions
    @previous = nil                  # no unflushed output exists 
  end

  def flush
    puts @previous if @previous
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
      @previous = "    SendDragonKeys #{expression}"
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
    return nil
  end



  def returns_int(closure)
    proc { |name,args|
             return "CStr(#{closure.call(name, args)})"
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

        "AppBringUp"		=> procedure,
	"ButtonClick"		=> int(int(procedure, 0), 1),
        "HeardWord"		=> procedure,
	"MouseGrid"		=> int(int(procedure, 0), 1),
        "SendSystemKeys"	=> procedure,
	"SetMousePosition"	=> int(int(int(procedure, 0), 1), 2),
        "Shell"			=> procedure,

        "Len"			=> returns_int(function),
        "Right$"		=> int(function, 1),
        "Replace$"		=> function,

	"First"		=>
             proc { |name,args|
              "Left(#{args[0]}, InStr(#{args[0]} & #{args[1]}, #{args[1]})-1)"
                  },
	"Second"		=>
             proc { |name,args|
              "Mid$(#{args[0]}, InStr(#{args[0]} & #{args[1]}, #{args[1]})+1)"
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
      0.upto(formals.length-1) do |i|
#$stderr.puts "#{args[i]} for #{formals[i]}"
	body = body.collect do |token|
	  token=="$#{formals[i]}" ? '"" & ' + args[i] + ' & ""' : token
	end
      end

      @tokens = body + @tokens
      nil
    end
  end
end
 
Compile.new.main
