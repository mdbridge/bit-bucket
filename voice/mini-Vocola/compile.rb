TARGET = "to_DNS.xml"

out = File.open(TARGET, "w")


# 
# Header:
# 
out.puts <<HEADER
<?xml version="1.0" encoding="windows-1252"?>

<!DOCTYPE MyCommands SYSTEM "http://www.scansoft.com/NaturallySpeaking/Support/MyCommands/MyCmds11.dtd">

<MyCommands version="2.0" language="0x409">

HEADER


#
# Process each command file:
#
ARGV.each do |argument|
  next unless argument =~ /\.mv$/
  file = argument

  puts "  compiling #{file}..."
  name = file.sub(%r_.*/_, "").sub(/[_.].*/, "").sub(/^$/, "_vocola")

  if File.exist?("contexts/#{name}")
    out.puts File.read("contexts/#{name}")
  else
    $stderr.puts "Warning: unknown context '#{name}'"
    out.puts <<CONTEXT
	<Commands type="application">
		<module>#{name}</module>
		<company>Unknown</company>
		<description>Unknown: #{name}</description>
CONTEXT
  end
  out.puts

  out.puts `ruby -w compile_main.rb "#{file}"`

  out.puts <<FILE_TRAILER
	</Commands>
FILE_TRAILER

end



#
# Process each list file:
#

out.puts <<LISTS_HEADER

	<Lists>
LISTS_HEADER

ARGV.each do |argument|
  next unless argument =~ /\.list$/
  file = argument
  name = file.sub(%r_.*/_, "").sub(/\.list$/, "").gsub(/_/, "-")

  #puts "  compiling #{file}..."
  out.puts "		<List name=\"mv-#{name}\">"
  File.readlines(file).each do |alternative|
    alternative.chomp!
    next if alternative =~ /^\s*$/

    if alternative =~ /(.*)\\([^\\]*)/ then
      written = $1
      spoken  = $2
    else
      written = spoken = alternative
    end
    written = written.gsub(/\&/, "&A").gsub(/\\/, "&B")
    alternative = "#{written}\\#{spoken}"
    alternative = spoken if written == spoken

    out.puts "\t\t\t<Value><\![CDATA[#{alternative}]]></Value>"
  end
  out.puts "		</List>"
  out.puts
end

out.puts <<TRAILER
	</Lists>

</MyCommands>
TRAILER
