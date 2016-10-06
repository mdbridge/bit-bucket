## 
## Parsing word specifications:
## 

def each_word(source)
  source.each do |line|
    line.gsub!(/^(([^\#\\]|\\.|\\\Z)*).*/, '\1').chomp!   # eat comments, EOL
    next if line =~ /^\s*$/

    yield(*line.scan(/(?:\A|:)(([^\:\\]|\\.|\\\Z|)*)/)
          .collect { |f,| f.gsub(/\\(.)/, '\1').strip })
  end
end


## 
## Compilation:
## 

def compile(source, command, only_with_properties=false)
  puts <<HEADER
# -*- encoding: windows-1252 -*-
###
### This file automatically generated via word_compiler.rb v0.2
### 

include "vocabulary_editor.vch";


#{command} =
HEADER

  each_word(source) do |written, spoken, properties|
    spoken.gsub!(/ +/, " ") if spoken
    properties = "" if not properties and $avoid_bug and spoken and written.downcase == spoken.downcase
    next if not properties and only_with_properties

    print "    Word(#{quote(written)}, #{quote(spoken || '')}) "
    if properties == "-"
      print "Delete()"
    elsif properties == "+"
      print "Add() {alt+p}{enter}"
    else
      print "Add()"
      if properties
        print " POpen() "
        print properties.gsub(/(\A|\s)(\w+)(\Z|\s)/, ' \2() ') 
        print " PClose()"
      end
    end
    puts
  end

  puts "    Done();"
end

def quote string
  '"' + string.gsub(/"/, '""') + '"'
end



## 
## Listing words without properties:
## 

def list_words(source, separator='\\\\')
  each_word(source) do |written, spoken, properties|
    spoken.gsub!(/ +/, " ") if spoken
    next if properties
    next if $avoid_bug and spoken and written.downcase == spoken.downcase

    print "#{written}"
    print "#{separator}#{spoken}" if spoken and spoken != ""
    print "\r\n"
  end
end



## 
## Main routine:
## 

Encoding.default_external = "Windows-1252"


def usage
  $stderr.puts "word_compiler.rb: usage: <option>* <filename>*"
  $stderr.puts "    <option> ::= -only[_with_properties] | -command <command_name>"
  $stderr.puts "               | -DNS<#> | -avoid_bug"
  exit 1
end

  # defaults:
only_with_properties = false
command              = "load compiled words"
separator            = nil
  # avoid bug where import words fails on "xyz\\Xyz" by using vocabulary
  # editor for those words:
$avoid_bug           = false

while ARGV.first =~ /^-/
  option = ARGV.shift
  case option
  when "-command"
    usage unless ARGV.length > 0
    command = ARGV.shift
  when /^-only/
    only_with_properties = true

  when "-avoid_bug"
    $avoid_bug = true

  when /^-DNS(\d+)(.\d+)?$/
    separator = '\\'
    if $1.to_i >= 11
      print "@Version=Plato\r\n"
      separator = '\\\\'
    end

  else
    $stderr.puts "word_compiler.rb: unrecognized option: #{option}\n\n"
    usage
  end
end


if separator
  list_words(ARGF, separator)
else
  compile(ARGF, command, only_with_properties)
end
