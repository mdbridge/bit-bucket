###
### Extract work e-mail addresses from Outlook export file (was tab
### separated, now CSV) run through escape program:
### 
### Uses LDAP when necessary.
### 
### The following extra fields are used in contact information:
### 
###   User 1: alternative spoken name for nickname (default is first name)
###   User 2: if present, additional nickname
### 

def split_Outlook text
  # CSV (after escape, all non-empty values quoted):
  (text+",").scan(/(?:\"[^\"]*\")?,/).
    collect { |x| x[0..-2].gsub(/^\"(.*)\"$/, "\\1") }

  # tab separated:
  #text.split(/\t/).collect { |x| x.gsub(/^\"(.*)\"$/, "\\1") }
end


field_names = split_Outlook(gets.chomp).collect { |x| x.gsub(/"/, "") }

f = Hash.new()
field_names.each_index { |x| f[field_names[x]] = x }

#puts field_names.inspect



while line = gets() do
  fields     = split_Outlook(line.chomp)
  #$stderr.puts fields.inspect
  first_name = fields[f["First Name"]]
  last_name  = fields[f["Last Name"]]
  name       = "#{first_name} #{last_name}".gsub(/ +$/, "")

  nickname   = fields[f["User 1"]]
  nickname   = first_name if !nickname || nickname == ""

  nickname2  = fields[f["User 2"]]


  # 
  # attempt to get email address:
  # 
  type = fields[f["E-mail Type"]]
  case type
  when ""       # No email listed at all...
    next         

  when "SMTP"
    email = fields[f["E-mail Address"]]
    
  when "EX"
    display = fields[f["E-mail Display Name"]]
    if display =~ /\(([^)]*@.*)\)/ then
      email = $1
    else
      #$stderr.puts name
        # first look up by first, last name:
      email = `lookupEmailLDAP.pl 'cn=#{first_name}*#{last_name}'`.chomp
      if email =~ /^FAIL/ then
        # then try by username:
        email = `lookupEmailLDAP.pl 'ntUserDomainId=*:#{fields[f["Account"]]}'`.chomp
      end
      if email =~ /^FAIL/ then
        fail "ERROR: unable to find email for #{first_name} #{last_name}"
      end
    end
    
  else
    fail "unknown email type: #{type} for #{first_name} #{last_name}"
  end

  puts "#{email}\\#{name}"
  puts "#{email}\\#{nickname}"    unless name == nickname
  puts "#{email}\\#{nickname2}"   if nickname2
end
