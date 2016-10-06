### 
### Attempt to extract symbol definitions from C headers, minimizing
### false positives
### 
### Not intended to be used on .c files or C++ files
### 

def symbol(*arguments)
  $symbols.push(arguments)
  #$stderr.puts arguments.join("\t")
end
$symbols = []


def process(filename, text)

  # remove preprocessor directives, extracting symbol definitions when found:
  text = text.gsub(%r< ^ [ \t\f]* \# 
                       ([^\n\\]* \\ \n)* [^\n\\]* \n >mx) { |directive|
    if directive =~ /^\s*\#\s*define\s+(\w+)(\s|\s*[^\s\n])/ then
      #puts "@"+directive
      define  = $1
      context = $2
      if define !~ /^_/ then
        symbol(define, "#define", context, filename)
        #"\#define #{define}\n"
        "\n"
      else
        "\n"
      end
    else
      #puts "!"+directive
      "\n"
    end
  }


  # remove words from within C lexical pseudo tokens:
  text = text.gsub(%r< \'(\\.|[^\\\'])*\' 
                     | \"(\\.|[^\\\"])*\" 
                     | //[^\n]*\n 
                     | /\* .*? \*/ 
                     >mx) { |pseudo_token|
    if pseudo_token =~ /^[\'\"]/ then
      pseudo_token[0] * 2
    elsif pseudo_token[1] == "/" then
      "\n"
    else
      " "
    end
  }


#  text = text.gsub(%r< \( ( [^\)] | \(\) )+ \) >mx, "()")
#  text = text.gsub(%r< \( ( [^\)] | \(\) )+ \) >mx, "()")
#  text = text.gsub(%r< \( ( [^\)] | \(\) )+ \) >mx, "()")
#  text = text.gsub(%r< \( ( [^\)] | \(\) )+ \) >mx, "()")

  text.scan(/\w+/) do |x|
    if x.length != 1 and x !~ /^[\d_]/ and x != "enum" then
      symbol(x, "freestanding", filename)
    end
  end



end






alternative_includes = []
alternative_includes += [`echo /usr/lib/gcc/*/*/include`.chomp.split[0]]
alternative_includes += [`echo /usr/include/x86_64-linux-gnu`.chomp.split[0]]
#$stderr.puts alternative_includes.inspect

code = ""
ARGV.each do |file|
  if not File.exist?(file) then
    alternative_includes.each do |alternative_include|
      alternative = file.gsub("/usr/include", alternative_include)
      if File.exist?(alternative) then
        #$stderr.puts "#{file} -> #{alternative}"
        file = alternative
        break
      end
    end
  end

  if not File.exist?(file) then
      $stderr.puts "Warning: unable to locate #{file}"
      next
  end

  code += process(file, File.read(file))
end

#code = ARGF.read()

#code.gsub!(%r< \'(\\.|[^\\\'])*\' | \"(\\.|[^\\\"])*\" | //[^\n]*\n 
#             | /\* .*? \*/ >mx, " ")

# deal with 0.e13[fFlL] if needed later

#symbols = code.scan(/\w+/).reject { |x| x =~ /^\d/ || x.length==1 }
#
#puts symbols.sort_by { |x| x.downcase }.uniq




#code = process("empty", code)
#puts code

#puts $symbols.sort_by { |x| x[0].downcase }.uniq.collect { |x| x.join("\t") }
puts $symbols.collect { |x| x[0] }.sort_by { |x| x.downcase }.uniq
