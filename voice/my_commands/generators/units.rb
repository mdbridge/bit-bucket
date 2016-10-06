### 
### Automatically generate list of files from selected directories
### 

require '~/voice/my_commands/generators/priority_list'



Substitutions = <<HERE.split(/\n/).reject { |x| x =~ /^\#/ }.collect { |x| x.split(/,\s*/) }
# Express:
alloc,allocate
bf,BF
btcursor,B-tree cursor
btree,B-tree
cb,control block
chkpt,checkpoint
compt,compatibility
du,DU
h,handle
idx,index
impl,implementation
io,I/O
logrec,log record
logstub,log stub
sm,SM
smstats,SM stats
smthread,SM thread
stnode,store node
sysdefs,system definitions
vol,volume
vtable,V table
xct,transaction
#
# bold:
dclcrwlock,D lock
fba,FBA
HERE


def add_file(list, file, prefix, directory="~/voice/my_commands/")
  return if file =~ /__/

  spoken = file.sub(/\.[a-z]+$/, "")
  spoken = spoken.sub(%r|^.*/|, "")

  spoken.gsub!(/_/, " ")
  Substitutions.each { |w,s| spoken.gsub!(/\b#{w}\b/,s) }

  priority =    0
  priority -= spoken.length

  add_with_abbreviations(list, "#{prefix}#{spoken}", "#{directory}#{file}", 
                         priority)
end


def add_with_abbreviations(list, spoken, written, priority=0, full=true)
  # full version beats any abbreviated one:
  p = priority + (full ? 100000 : 0)
  list.add(spoken.gsub(/_/, " "), written, p)

  add_with_abbreviations(list, $1, written, priority, false) if spoken =~ /[^_]*_(.*)/
  add_with_abbreviations(list, $1, written, priority, false) if spoken =~ /(.*)_for (home|work)$/
end




#$directories = ["deduplication/tube/Eiger/auction_simulator/"]
#$directories = ["deduplication/tube/Eiger/metadata/"]
#$directories = ["past/HP/express/repository/Zero/src/sm/"]
$directories = ["the_machine/bold/src/",
                "the_machine/bold/include/**/", 
                "the_machine/bold/examples/",
                "the_machine/bold/tests/**/"]


$units     = PriorityList.new()
$interface = PriorityList.new()

$directories.each do |directory_pattern|
  Dir["/home/#{ENV["USER"]}/#{directory_pattern}"].each do |directory|
    Dir["#{directory}*"].each do |file|
      fail unless file =~ %r{/home/#{ENV["USER"]}/(.*)/([^/]*)$}
      folder   = "work:~/#$1/"
      filename = $2

      case filename
      when /\.cpp$/
        add_file($units, filename, "", folder)

      when /\.h$/, /\.hpp$/
        add_file($interface, filename, "", folder)
      end
    end
  end
end

  
$units.make("units")
$interface.make("interface")
