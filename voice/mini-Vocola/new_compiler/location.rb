# 
# A character in a file suitable for use in complaining about source files
# 
# or, special case, an entire file
#
class Location
  def initialize(text, line, offset, pathname = "<unknown>")
    @text = text           # text of line holding our character (no \n or \r)
                           # or nil if we represent an entire file

    @line = line           # line number of line holding our char (base 1)
    @offset = offset       # offset of our character in text (base 0)
    @pathname = pathname   # human readable name of file holding our character 
  end

  attr_reader :text, :line, :offset, :pathname

  def Location.whole_file(pathname = "<unknown>")
    Location.new(nil, 0, 0, pathname)
  end
end 


module Error
  $errors = 0
  $warnings = 0


  def Error.error(location, message, out=$stderr)
    Error.report(location, "error", message, out)
    $errors += 1
  end

  def Error.warning(location, message, out=$stderr)
    Error.report(location, "warning", message, out)
    $warnings += 1
  end

  def Error.report(location, type, message, out=$stderr)
    if location.text then
      out.puts "#{location.pathname}:#{location.line}: #{type}: #{message}"
      out.puts location.text.gsub(/\t/, " ")
      out.puts "#{' '*location.offset}^"
    else
      out.puts "#{location.pathname}: #{type}: #{message}"
    end
  end
end

