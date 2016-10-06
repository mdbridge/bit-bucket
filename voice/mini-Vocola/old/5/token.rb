module Error
  $errors = 0

  def Error.report pathname, line_number, message
    $stderr.puts "#{pathname}:#{line_number}: #{message}\n\n"
    $errors += 1
  end
end

class Tokens
  def initialize(pathname)
    @pathname = pathname
    @line_number = 0
  end

  attr_reader :pathname, :line_number

  def each
    File.open(@pathname) do |file|
      @line_number = 0
      file.each do |line|
        @line_number += 1

        # get tokens and whitespace pseudo tokens:
        pseudo_tokens = \
          line.chomp.scan(/"[^"]*"?
                          |'[^']*'?
                          |\#.*
                          |\s+
                          |[()\[\]=|,;]
                          |:=
                          |:  [^"'\#\s ()\[\]=|,; :]*
                          |[^"'\#\s ()\[\]=|,; :] [^"'\#\s ()\[\]=|,; :]*
                          /x)                                              #" 

        # remove whitespace pseudo tokens, leaving only real tokens 
        tokens = pseudo_tokens.reject {|pseudo| pseudo =~ /^(\#.* | \s+)$/x }

        tokens.each { |token| yield process_token(token) }
      end
    end
    nil
  end

  def process_token token
    balanced = case token
      when /^".*[^"]$/
        Error.report @pathname, @line_number, 'unterminated double quote'
        token + '"'
      when /^'.*[^']$/
        Error.report @pathname, @line_number, 'unterminated single quote'
        token + "'"
      else
        token
    end

    # convert single quotes to Visual-Basic-style double quotes:
    balanced = '"' + (balanced[1..-2].gsub(/"/,'""')) + '"' if balanced =~ /^'/

    balanced.gsub!(/\t/, "{tab}")

    balanced
  end
end

class Statements
  def initialize(pathname)
    @pathname = pathname
    @starting_line = 0
  end

  attr_reader :pathname, :starting_line
  
  def each
    accumulator = []
    @starting_line = 0

    tokens = Tokens.new(@pathname)
    tokens.each do |token|
      @starting_line = tokens.line_number if accumulator.length == 0
      accumulator << token
      next unless token == ";" || token == ":"

      yield accumulator
      accumulator = []
    end
    
    if accumulator.length != 0
      Error.report @pathname, @starting_line,
        "unterminated statement:\n#{accumulator.join(" ")}"
    end
    
    nil
  end
end

#s = Statements.new("_temporary.mv")
#s.each {|x| puts x.inspect , s.starting_line }

Statements.new(ARGV[0]).each {|x| puts x[0...-1].join("\t") }
