require "./rolodex"

require 'cgi'


# feed me HTML source of address book view all page
def extract html
  results = Rolodex.new
  
  while line = html.gets
    if line =~ /editContact/ && line =~ /title="\s*([^"]*?)\s*"/
      name = $1 
      name = CGI::unescapeHTML(name)
      name = "" if name == "[Edit Details]"
      name = reverse_name(name) if name
    end

    if line =~ /"mailto:([^"]*)"/
      email = $1

      if name != ""
        results.add({ "name" => name, "email" => email, "source" => ["evite"]})
      else
        $stderr.puts "missing name for email address: #{email}"
      end
    end
  end

  results
end
    
#rolodex = extract(File.open("evite.htm"))
rolodex = extract($<)

rolodex.dump
