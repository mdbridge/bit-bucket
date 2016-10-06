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


  # special tokens: ( ) [ ] = | , ; : :=
  # normal tokens:  /"[^"\n]"/  /'[^'\n]'/  /(\X|:)*\X/
  #    where \X denotes the character class [^"'\#\s#{Specials}:]
  #      with Specials being the list of special characters (defined below)
  #
  # no other tokens exist
  def Tokens.special? token
    return token =~ /^([#{Specials}]|:|:=)$/
  end
  Specials = '()\[\]=|,;'    # The special characters (does not include '\')


  def each
    @line_number = 0
    begin
      File.open(@pathname) do |file|
        file.each do |line|
          @line_number = file.lineno
          
          # get tokens and whitespace pseudo tokens:
          pseudo_tokens = \
            line.chomp.scan(/"[^"]*"?
                            |'[^']*'?
                            |\#.*
                            |\s+
                            |[#{Specials}]
                            |[^"'\#\s #{Specials}]* [^"'\#\s #{Specials} :]
                            |:=
                            |:
                            /x)                                             #" 

          # remove whitespace pseudo tokens, leaving only real tokens 
          tokens = pseudo_tokens.reject {|pseudo| pseudo =~ /^(\#.* | \s+)$/x }
          
          tokens.each { |token| yield process_token(token) }
        end
      end
    rescue SystemCallError
      Error.report @pathname, @line_number+1, "error reading: " + $!
    end

    nil
  end

  def process_token token
    balanced = case token
      when /^".*[^"]$/
        Error.report @pathname, @line_number,
                 "unterminated double quote: #{token}"
        token + '"'
      when /^'.*[^']$/
        Error.report @pathname, @line_number,
                 "unterminated single quote: #{token}"
        token + "'"
      else
        token
      end

    # convert single quotes to Visual-Basic-style double quotes:
    balanced = '"' + (balanced[1...-1].gsub(/"/,'""')) + '"' if balanced =~ /^'/

    balanced.gsub!(/\t/, "{tab}")#<<<>>>

    balanced
  end

  def Tokens.unquote token
    return token[1...-1].gsub(/""/, '"') if token =~ /^"/
    return token
  end
end



class Statements
  def initialize(pathname)
    @pathname = pathname
    @starting_line = 0
    @statement = nil
  end

  attr_reader :pathname, :starting_line

  def error message
    Error.report @pathname, @starting_line,
      "#{message}:\n  #{@statement.join(" ")}"
  end


  def each
    @starting_line = 0
    @statement = []

    # <pseudo-statement> ::= <x>* (':' | ';')
    #    where <x> is any token other than ':' or ';'
    #
    # (all <foo>'s are <pseudo-foo>'s, but not all <pseudo-foo>'s are <foo>s)
    tokens = Tokens.new(@pathname)
    tokens.each do |token|
      @starting_line = tokens.line_number if @statement.length == 0
      @statement << token
      next unless token == ";" || token == ":"

      process_statement { |s| yield s }
      @statement = []
    end
    
    error("unterminated statement") if @statement.length != 0
    nil
  end


  def process_statement
    if directive? != "include"
      yield @statement
      return
    end

    #
    # handle directive: 'include' <filename> ';'
    #   where <filename> is one normal token.
    #

    if @statement.length != 3
      error "include directive has too many arguments"
      return
    end

    filename = Tokens.unquote(@statement[1])
    if !File.exists?(filename)
      error "no such file: #{filename}"
      return
    end

    # return if we have already included this file? <<<>>>
    
    begin
      saved_starting_line = @starting_line
      saved_pathname = @pathname

      @pathname = filename
      each {|statement| yield statement}
    ensure
      @starting_line = saved_starting_line
      @pathname = saved_pathname
    end
  end


  def directive?
    # <directive> ::= <normal>+ ';'
    #    where <normal> is any non-special token
    return nil if @statement.length < 2
    return nil if @statement[-1] != ";"
    return nil if @statement[0...-1].any?{|t| Tokens.special?(t)}

    return @statement[0]    # return directive's name
  end
end

#s = Statements.new("_temporary.mv")
#s.each {|x| puts x.inspect , s.starting_line }

Statements.new(ARGV[0]).each {|x| puts x[0...-1].join("\t") }
