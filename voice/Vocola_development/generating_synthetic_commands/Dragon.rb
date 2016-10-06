#COUNT = 1000
COUNT = 10000

words = File.read("/usr/share/dict/american-english-huge").scan(/^[A-Za-z]+$/)

samples = 0.upto(COUNT).collect { |x| words.sample(3) }


$stdout.set_encoding(Encoding::UTF_16)



puts <<HEADER
<?xml version="1.0" encoding="utf-16"?>

<!DOCTYPE MyCommands SYSTEM "http://www.nuance.com/NaturallySpeaking/Support/MyCommands/MyCmds11.dtd">

<MyCommands version="2.0" language="0x409">


	<Commands type="application">
		<module>notepad</module>
		<company>Microsoft Corporation</company>
		<description>Notepad</description>

HEADER

samples.each do |first, second, third|
  #puts "command #{first} #{second} #{third} = '#{first} #{second} #{third}';"
puts <<"COMMAND"
		<Command name="command #{first} #{second} #{third}" group="test" enabled="true" states="">
			<description></description>
			<contents type="SCRIPT">
<![CDATA[Sub Main
	SendDragonKeys "#{first} #{second} #{third}"
End Sub
]]>
			</contents>
		</Command>
COMMAND
end

puts <<TRAILER
	</Commands>

	<Lists>

	</Lists>

</MyCommands>
TRAILER
