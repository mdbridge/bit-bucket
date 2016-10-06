##
## Code to extract rolodex entries for evite:
##

require './rolodex'


#
# read in rolodex:
#
R = Rolodex.load(File.open("addresses.rol"))
#R = Rolodex.load(File.open("HP.rol"))


#
# extract only people:
#
E = R.reject { |x| x["type"] }


#
# Output selected fields to a .csv file suitable for import into Evite.com or 
# other programs:
#
File.open("to_evite.csv", "w") do |output|
  output.puts "Last Name, First Name, Email Address, Email 2 Address, Mobile Number, Home Number, Work Number, Other Number, Spouse\r"

  E.each do |entry|
    name   = entry["name"]
    email  = get_email(entry)[0] || ""
    email2 = get_email(entry)[1] || ""

    if name !~ /,/ then
      name = "," + name

      livejournal = entry["livejournal"]
      name = "(LJ #{livejournal})" + name if livejournal
    end


    cell   = entry["cell"] || ""
    home   = entry["home"] || ""
    work   = entry["work"] || ""
    phone  = entry["phone"] || ""
    
    spouse = entry["spouse"] || ""
    spouse = restore_name(spouse) if spouse =~ /,/

    # Skip entries without email addresses:
    if email == "" then
      puts "Skipping #{name} due to no email..." 
      #next
    end


    output.puts "#{name}, #{email}, #{email2}, #{cell}, #{home}, #{work}, #{phone}, #{spouse}\r"
  end
end
