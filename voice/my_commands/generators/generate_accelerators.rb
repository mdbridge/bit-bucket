### 
### Generating accelerators HTML files & lists of folders from saved
### bookmark files
### 

require 'haml'

require '~/voice/my_commands/generators/list_generator'


## 
## Reading in and parsing a bookmarks file:
## 

class Folder
  attr_reader :links, :subfolders

  def initialize links, subfolders
    @links, @subfolders = links, subfolders
  end


  def Folder.parse_file filename
    parse(File.open(filename, "r:utf-8").read.gsub(/\r/,""))
  end

  def Folder.parse text
    lines = text.split(/\n/)
    parse_folder lines
  end

  def Folder.parse_folder lines
    links      = []
    subfolders = {}

    while lines.shift !~ /<DL>/i and lines.size>0 do end

    while line = lines.shift
      if line =~ /^\s*<DT><A/i
        text, url = parse_link line
        next if url =~ /^place:/
        links << [text, url]
      elsif line =~ %r{<H3 .*?>(.*?)</H3>}i
        name = $1
        subfolders[name] = parse_folder lines
      elsif line =~ /<\/DL>/i 
        break
      end
    end

    Folder.new links, subfolders
  end

  # text, url =
  def Folder.parse_link text
    fail unless text =~ %r{<a href=\"(.*?)\".*?>(.*?)</a>}i
    text, url = $2, $1
    text.sub!(/\.URL$/i, "")   # fix Internet Explorer bug...
    return text, url
  end
end


## 
## Generating an accelerators HTML file from bookmarks:
## 

def generate_HTML(name, folder)
  haml_engine = Haml::Engine.new(File.read("accelerators-template.html.haml"))
  haml_engine.render(folder, kind: name)
end


## 
## Generating a list of folders:
## 

def list_of_folders(folder)
  entries = [["top level", "top"]] + folder_entries(folder, "")
  TrivialList.new(entries)
end

def folder_entries folder, prefix=""
  folder.subfolders.collect do |name, subfolder| 
    [[name, prefix+name]] + folder_entries(subfolder, "second_")
  end.flatten(1)
end


## 
## Main routine:
## 

fail unless ARGV.length == 1
Name = ARGV[0]


top_folder = Folder.parse_file("/home/#{ENV['USER']}/backups/bookmarks/Chrome-#{Name}.htm")
#folders.sort! if Name == "work"  # needed for IE

File.open("/home/#{ENV['USER']}/Tmp/#{Name}-accelerators.html", "w") do |out|
  out.puts generate_HTML(Name.capitalize(), top_folder)
end

list_of_folders(top_folder).make("#{Name}_bookmark_folder", 
                                 "#{Name}_bookmark_folders")
