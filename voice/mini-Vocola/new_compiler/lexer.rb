require 'location.rb'

class TokenStream
  ## 
  ## Reading from files; returns nil if I/O error
  ## 

  def TokenStream.from_file(pathname)
    begin
      input = File.open(pathname)
    rescue SystemCallError
      Error.error(Location.whole_file(pathname), "unable to open: " + $!)
      return nil
    end

    begin
      return TokenStream.from_source(input, pathname)
    ensure
      input.close
    end
  end
  
  def TokenStream.from_source(source, source_name = "<unknown>")
    begin
      string = source.read
      string = "" if !string
      return TokenStream.new(string, source_name)
    rescue SystemCallError
      Error.error(Location.whole_file(source_name),
                  "problem reading file: " + $!)
      return nil
    end
  end

  def TokenStream.from_stdin
    TokenStream.from_source($stdin, "<stdin>")
  end


  ## 
  ## Lexing
  ##
  ##   Note: the text lexed always ends in a newline.
  ## 

  # 
  # These pseudo tokens "hide" the characters in them:
  # 

  Comment   = /\# [^\n]* \n/x

    # Note that these may be (illegally) unterminated:
  Quotation = /\' (?:[^\'\n]|'')* [\'\n]
              |\" (?:[^\"\n]|"")* [\"\n]
              /x

  Protected_pseudo_token = /#{Comment} | #{Quotation}/x


  # 
  # The special characters that when unhidden always start a new 
  # protected pseudo token:
  #
  Start = '\'\"\#'

  #
  # The special characters, each of which when unhidden in a non-context 
  # statement comprises a single token by itself:
  #
  Singles = '()\[\]|,;'


  # 
  # Support for legacy context statements:
  # 

    # below definitions assume outside any protected pseudo token:
  Context_colon = /: (?! = | :*[^\s#{Start}#{Singles}=:] ) /x
  Normal_colon  = /: (?= = | :*[^\s#{Start}#{Singles}=:] ) /x

  In_context    = /(?= (?: [^#{Start};] | #{Protected_pseudo_token})* 
                      #{Context_colon}                            )/x

  Char          = /[^\s#{Start};|:] | #{Normal_colon}/x

  Context_bare_word = /#{In_context}
                       #{Char}
                       (?: (?: #{Char}|\s )* #{Char} )?
                      /x


  # 
  # Every string ending with a newline can be divided into a continuous 
  # series of these, with each pseudo token being as greedy as possible.
  # 
  Pseudo_token = /\s+
                 |#{Protected_pseudo_token}
                 |[#{Singles}]
                 |#{Context_bare_word}
                 |[^\s#{Start}#{Singles}=]* [^\s#{Start}#{Singles}=:]
                 |:=
                 |=
                 |:
                 /x


  def initialize(string, source_name = "<unknown>")
    # fixup line terminators to one newline per line:
    string = string.gsub(/\r\n/, "\n")
    string += "\n" if string !~ /\n\z/
    lines = string.chomp.split(/\n/)

    line_number = 1
    offset      = 0
    @tokens = []

    string.scan(Pseudo_token) do |token|

      if token !~ /^[\s\#]/ then
        l = Location.new(lines[line_number-1], line_number, offset, source_name)

#        print "_#{token}"
#        puts if token =~ /^;|:$/
        Error.warning(l, "token is <#{token}>")
      end

      token.scan(/\n/) { line_number += 1; offset = 0 }
      offset += token[/[^\n]*\z/].length
    end
  end
end



TokenStream.from_stdin()
