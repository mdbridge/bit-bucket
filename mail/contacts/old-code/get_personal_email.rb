##
## 
##

require './rolodex'


#
# read in rolodex:
#
R = Rolodex.load(File.open("addresses"))

R.each do |person|
  next if person["type"] 

  name = person["name"]
  next if !name

  name = restore_name(person["name"])

  email = get_email(person)
  next if email.length == 0
  email = email[0]

  puts "#{email}\\#{name}"

  spoken = person["spoken"]
  puts "#{email}\\#{spoken}" if spoken
end
