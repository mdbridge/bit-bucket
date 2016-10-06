### 
### "cleanup" a human-edited Rolodex by loading then dumping it
### 

require "./code/rolodex"


fail unless ARGV.size == 1
Rolodex_name = ARGV[0]

begin
  R = Rolodex.load(File.open(Rolodex_name))
rescue RolodexError => e
  $stderr.puts "***** Error: #{e}"
  exit 1
end

R.dump
exit 0
