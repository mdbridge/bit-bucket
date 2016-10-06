### 
### Extract personal email address, name pairs in DNS format from Rolodex
### 

require './code/rolodex'


R = Rolodex.load(File.open("addresses.rol"))

R.each do |person|
  next if person.type

  email = person.get_all_email[0]
  next unless email

  name   = restore_name(person.key)
  spoken = person.spoken

  puts "#{email}\\#{name}"
  puts "#{email}\\#{spoken}" if spoken
end
