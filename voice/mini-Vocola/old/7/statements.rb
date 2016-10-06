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
  end

  attr_reader :pathname


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
    line_number = 0
    begin
      File.open(@pathname) do |file|
        file.each do |line|
          line_number = file.lineno
          
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
          
          tokens.each do |token|
            yield process_token(token,line_number), line_number
          end
        end
      end
    rescue SystemCallError
      Error.report @pathname, line_number+1, "error reading: " + $!
    end

    nil
  end

  def process_token token, line_number
    balanced = case token
      when /^".*[^"]$/
        Error.report @pathname, line_number,
                 "unterminated double quote: #{token}"
        token + '"'
      when /^'.*[^']$/
        Error.report @pathname, line_number,
                 "unterminated single quote: #{token}"
        token + "'"
      else
        token
      end

    # convert single quotes to Visual-Basic-style double quotes:
    balanced = '"' + (balanced[1...-1].gsub(/"/,'""')) + '"' if balanced =~ /^'/

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
  end

  attr_reader :pathname


  def each(pathname = @pathname)
    starting_line = 0
    tokens = []

    # <pseudo-statement> ::= <x>* (':' | ';')
    #    where <x> is any token other than ':' or ';'
    #
    # (all <foo>'s are <pseudo-foo>'s, but not all <pseudo-foo>'s are <foo>s)
    Tokens.new(pathname).each do |token, line_number|
      starting_line = line_number if tokens.length == 0
      tokens << token
      next unless token == ";" || token == ":"

      statement = Statement.new(pathname, starting_line, tokens)
      tokens = []

      process_statement(statement) { |s| yield s }
    end
    
    if tokens.length != 0
      Statement.new(pathname, starting_line, tokens) \
          .error("unterminated statement") 
    end

    nil
  end


  def process_statement statement
    if statement.directive? != "include"
      yield statement
      return
    end

    #
    # handle directive: 'include' <filename> ';'
    #   where <filename> is one normal token.
    #

    if statement.tokens.length != 3
      statement.error "include directive has too many arguments"
      return
    end

    filename = Tokens.unquote(statement.tokens[1])
    if !File.exists?(filename)
      statement.error "no such file: #{filename}"
      return
    end

    # return if we have already included this file? <<<>>>
    
    each(filename) {|statement| yield statement}
  end
end



class Statement
  def initialize pathname, starting_line, tokens
    @pathname = pathname
    @starting_line = starting_line
    @tokens = tokens.freeze
  end

  attr_reader :pathname, :starting_line, :tokens


  def error message
    Error.report @pathname, @starting_line,
      "#{message}:\n  #{@tokens.join(" ")}"
  end


  def directive?
    # <directive> ::= <normal>+ ';'
    #    where <normal> is any non-special token
    return nil if tokens.length < 2
    return nil if tokens[-1] != ";"
    return nil if tokens[0...-1].any?{|t| Tokens.special?(t)}

    return tokens[0]    # return directive's name
  end
end
