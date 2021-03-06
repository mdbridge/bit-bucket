### 
### A superclass for classes representing Vocola lists
### 
### Knows how to handle pretty printing a list of spoken, written, disable? entries
###   (sorting, aligning things, quoting, commenting out disabled entries)
### 

require '~/voice/my_commands/generators/Vocola_quoting'


class ListGenerator
  attr_accessor :sort_by_written  # alternative is sort by spoken
  attr_accessor :do_not_infer     # if true, always provide a written form

  def initialize
    @sort_by_written = false
    @do_not_infer    = false
  end


  def make(name, file_name=name, directory="/home/#{ENV['USER']}/voice/my_commands/commands")
    File.open("#{directory}/#{file_name}.vch", "w") do |out|
      out.puts ListGenerator.banner

      out.puts generate(name)
    end
  end

  def ListGenerator.banner
    "###
### WARNING: this file was automatically generated by #$0!
###
### Do not edit!
###


"
  end


  def generate(name)
    entries = sort(generate_entries())

    entries = entries.collect do |spoken, written, disable| 
      quoted_spoken, inferred_written = VocolaQuoting.quote_spoken(spoken)
      quoted_spoken = " " + quoted_spoken   unless quoted_spoken =~ /^[\'\"]/

      actual_written = VocolaQuoting.quote_interpolation(written) 
      actual_written = nil if inferred_written==written and !@do_not_infer

      [quoted_spoken, actual_written, disable]
    end

    max_size = entries.collect { |spoken,w,d| spoken.length }.max

    indent    = " "*5;
    result    = "<#{name}> := (\n"
    first_one = true
    entries.each do |spoken, written, disable|
      if written then
        alternative = (spoken+" "*max_size)[0..max_size-1] + " = #{written}"
      else
        alternative = spoken
      end

      result     += indent + (disable ? "#" : " ") + (first_one ? "  " : "| ") + 
                    alternative + "\n"
      first_one   = false unless disable
    end
    result += ");\n"

    return result
  end


protected

  def sort entries
    key_index = (@sort_by_written ? 1 : 0)

    # this is a stable sort in spite of the fact that Ruby only
    # provides an unstable sort:
    i = 0
    entries.sort_by { |x| [canonicalize(x[key_index]), i += 1] }
  end

  def canonicalize key
    # sort small numbers in numerical order:
    if key =~ /^([0-9]+)(.*)/ then
      key = ("0"*10 + $1)[-9..-1] + $2
    end

    key.downcase
  end
end


class TrivialList < ListGenerator

  def initialize(entries)
    super()
    @entries = entries
  end

  def generate_entries()
    @entries
  end
end
