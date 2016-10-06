### 
### Convert saved bookmarks into a list of websites.
### 

require 'cgi.rb'

require '~/voice/my_commands/generators/list_generator'



def extract_bookmarks input
  results = []
  while (line = input.gets) do
    next if line !~ /<DT>/

    line.sub(%r_HREF=\"([^\"]*)\".*>([^<]*)</A>_) do
      spoken  = $2
      written = $1

      spoken.gsub!(/.URL$/i, "") # fix Internet Explorer bug...

      spoken.gsub!(/^ */, "").gsub!(/ *$/, "")
      spoken = CGI.unescapeHTML(spoken)
      results += [ [spoken, written] ]
    end
  end

  results
end



Name = ARGV[0]

puts TrivialList.banner

puts TrivialList.new(extract_bookmarks(File.open("../#{Name}.htm"))).
  generate("#{Name}_site")
