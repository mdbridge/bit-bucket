### 
### Extract work email address, name pairs in DNS format from Rolodex
### 

require "./code/rolodex"


R = Rolodex.load(File.open("addresses.rol"))

R.each do |entry|
  email = entry.work_email
  next unless email

  name      = restore_name entry.key
  nickname  = entry.nickname_1 || entry.nickname
  nickname2 = entry.nickname_2

  puts "#{email}\\#{name}"
  puts "#{email}\\#{nickname}"    unless name == nickname
  puts "#{email}\\#{nickname2}"   if nickname2
end
