### 
### Automatically generate lists of files of voice source (e.g.,
### Vocola, vocabulary).
### 
### Pass as arguments the filenames (e.g., letters.vch) of any Vocola
### include files that may not have been created yet.
### 

require '~/voice/my_commands/generators/priority_list'



def add_command_file(list, file, spoken_prefix="", written_prefix="")
  written = written_prefix + file

  basename = file.sub(%r|^.*/|, "").sub(/\.[a-z]+$/, "")

  spoken  = spoken_prefix + make_app_English(basename)


  # full version beats any abbreviated one:
  list.add(spoken, written, 100000)


  priority = 5000
  priority = 7000 if basename =~ /^gnu/
  priority = 6500 if basename =~ /^x_/
  priority = 6000 if basename =~ /^_/
  priority = 4000 if basename =~ /^nsbrowse/

  priority = 7500 if basename =~ /^_switch/

  components = spoken.split(/ /)
  (1..components.length()-1).each do |i|
    list.add(components[i..-1].join(" "), written, priority-i)
  end
  #list.sort_by_written = true
end


def make_app_English basename
  map_string = <<HERE
_any,anywhere
acrord32,Adobe_reader
applicationframehost,Microsoft edge
browser,Internet_browser
civilizationv,civ_five
cmd,command_prompt
dosbox,dos_box
hl2,portal
iexplore,Internet_explorer
leap3,leap_three
leap4,leap_four
lync,chat
mintty,Cygwin_terminal
nsbrowse,command_browser
powerpnt,PowerPoint
rx,reflection
sc2,StarCraft_two
sftpnetdrive,SFTP
taskmgr,task_manager
vbexpress,Visual_Basic_Express
Warcraft II,Warcraft_two
win32pad,water_pad
winword,Word
x,X
HERE
  map = Hash.new
  map_string.split(/\n/).collect { |x| x.split(/,/) }.each { |a,b| map[a]=b }

  base = basename.sub(/@mdl/, "_for home").sub(/@lillibridg3/, "_for work")
  base = basename.sub(/@@home/, "_home").sub(/@@work/, "_work")

  if base =~ /^(_any|[a-zA-Z0-9 ]+)([-_.].*|$)/ then
    name = "#{map[$1]||$1}#{$2}"
  elsif base =~ /^_([a-zA-Z0-9 ]+)([-_.].*|$)/ then
    name = "global_#{map[$1]||$1}#{$2}"
  else
    fail "malformed command file name: #{basename}"
  end

  #$stderr.puts "#{basename}->#{name.gsub('_', ' ')}"
  name.gsub("_", " ")
end



# Returns filenames, not pathnames:
def get_files(directory, pattern=/./)
  Dir.entries(File.expand_path(directory)).select { |x| x =~ pattern }.sort
end


def add_files(list, files, spoken_prefix="")
  files.each { |x| add_command_file(list, x, spoken_prefix) }
  list
end


def list_of_files(directory, pattern)
  add_files(PriorityList.new(), 
            get_files(directory, pattern).reject { |x| x =~ /^\./ })
end



## 
## Creation of the actual files:
## 

#add_files(list_of_files("mini-commands", /\.mv$/), 
#              get_files("mini-commands", /\.mvh$/), "include_") \
#    .make("macro_file", "macro_files")


list_of_files("commands", /\.v.l$/) \
    .make("command_file", "command_files")

add_files(list_of_files("commands", /\.vch$/), ARGV) \
    .make("include_file", "include_files")


list_of_files("../my_vocabulary/vocabulary", /\.txt$/) \
    .make("vocabulary_file", "vocabulary_files")
