### 
### Automatically generate map of person's name->their Rmail folder
### 

require '~/voice/my_commands/generators/priority_list'



folders = PriorityList.new()

base = "/home/#{ENV['USER']}/Rmail"

Dir.entries(base).each do |directory|
  next if directory =~ /\./            # skip ., .. directories
  path = "#{base}/#{directory}"

  next unless File.directory?(path)

  # skip work for now to keep list reasonable size:
  next if directory =~ /^[hsc]$/

  #puts directory


  Dir.entries(path).each do |folder|
    next unless folder =~ /^[a-zA-Z0-9.]+\.[a-zA-Z0-9]+$/
    #puts "  "+folder

    name = folder.split(/\./).collect { |x| x.capitalize }.join(" ")
    folders.add(name, directory + "/" + folder, 1)
  end
end


#puts folders.generate("personal_folder")
folders.make("personal_folder")
