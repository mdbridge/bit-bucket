### 
### 
### 

require './new_rolodex'



## 
## Parsing Outlook's formats:
## 

  # requires: input file produced via Outlook using 'CSV'
def read_Outlook_CSV pathname
  text  = IO.read(pathname, encoding:"Windows-1252").gsub(/\r/, "")
  lines = escape(text).split(/\n/)
  lines.collect { |x| split_Outlook_CSV(x) }
end

# encode newlines and tabs in quoted values by \\n and \\t's:
def escape text
  text.gsub(/\"([^\"]|"")*\"/) do |quote|
    quote.gsub(/\n/, "\\n").gsub(/\t/, "\\t")
  end
end

def split_Outlook_CSV text
  # Outlook quotes all nonempty values:
  (text+",").scan(/(?:\"[^\"]*\")?,/).
    collect { |x| x[0..-2].gsub(/^\"(.*)\"$/, "\\1") }
end


  # requires: input file produced via Outlook using 'Tab Separate Values (Windows)'
def read_Outlook_tabs pathname
  text  = IO.read(pathname, encoding:"Windows-1252").gsub(/\r/, "")
  lines = strip_quotes(text).split(/\n/)
  lines.collect { |x| x.split(/\t/) }
end

#
# encode newlines and tabs in quoted values by \\n and \\t's then
# remove quoting; quoting form is "...""..." for ..."...
#
def strip_quotes text
  text.gsub(/\"([^\"]|"")*\"/) do |quote|
    quote[1..-2].gsub(/\n/, "\\n").gsub(/\t/, "\\t").gsub(/""/, '"')
  end
end



## 
## Read in Outlook file, parse it, prepare for field lookup:
## 

#lines = read_Outlook_tabs("outlook.txt")
lines = read_Outlook_CSV("outlook.csv")

field_names = lines.shift
#puts field_names.inspect

$field_map = Hash.new()
field_names.each_with_index { |name,i| $field_map[name.downcase] = i }


def f(name, default=nil)
  index = $field_map[name]
  fail name unless index
  value = $fields[index]
  value=="" ? default : value
end

def address(prefix)
  return unless f(prefix + " street")

  full = f(prefix + " street")        + "\n" +
    f(prefix + " street 2", "")       + "\n" +
    f(prefix + " street 3", "")       + "\n" +
    f(prefix + " city", "")           + "\n" +
    f(prefix + " state", "")          + "\n" +
    f(prefix + " postal code", "")    + "\n" +
    f(prefix + " country/region", "") + "\n"

  "\n" + full.gsub(/\\n/, "\n").gsub(/^\n/, "")
end

def combine_names names
  name = ""
  names.each do |part|
    part = nil if part == ""
    name += " "  if part and name != ""
    name += part if part
  end
   name
end



## 
## Convert each Outlook entry to a RolodexEntry:
## 

entries = []
lines.each do |fields|
  $fields = fields
  entry   = RolodexEntry.new

  # 
  # Names:
  # 
  title       = f("title")
  first_name  = f("first name")
  middle_name = f("middle name")
  last_name   = f("last name")
  suffix      = f("suffix")       # usually used for disambiguation

  #name       = combine_names([first_name, last_name])
  first_part = combine_names([title, first_name, middle_name])
  last_part  = combine_names([last_name])
  fail unless first_part + last_part != ""

  full      = combine_names([first_part, last_part, suffix])

  reversed  = last_part
  reversed += ", " if reversed != "" and first_part != ""
  reversed += first_part

  entry.add!("reversed-name", reversed)

  #entry.add!("first", first_name)
  #entry.add!("name",          name)
  entry.add!("full-name",     full)

  nickname = f("user 1")
  nickname = first_name unless nickname
  entry.add!("nickname", nickname)
  entry.add!("nickname-2", f("user 2"))


  # 
  # Position information:
  # 
  entry.add!("job-title",  f("job title"))
  entry.add!("department", f("department"))
  entry.add!("company",    f("company"))
  entry.add!("assistant",  f("assistant's name"))


  # 
  # Phone numbers:
  # 
  entry.add!("cell",          f("mobile phone"))
  entry.add!("work",          f("business phone"))
  entry.add!("work2",         f("business phone 2"))
  entry.add!("home",          f("home phone"))
  entry.add!("home2",         f("home phone 2"))
  entry.add!("other-phone",   f("other phone"))
  entry.add!("primary-phone", f("primary phone"))


  # 
  # Email:
  # 
  type = f("e-mail type")
  case type
  when nil       # No email listed at all...
    email = nil

  when "SMTP"
    email = f("e-mail address")
    
  when "EX"
    display = f("e-mail display name")
    if display =~ /\(([^)]*@.*)\)/ then
      email = $1
    else
        # first look up by first, last name:
      email = `lookupEmailLDAP.pl 'cn=#{first_name}*#{last_name}'`.chomp
      if email =~ /^FAIL/ then
        # then try by username:
        email = `lookupEmailLDAP.pl 'ntUserDomainId=*:#{f("account")}'`.chomp
      end
      if email =~ /^FAIL/ then
        fail "ERROR: unable to find email for #{first_name} #{last_name}"
      end
    end
    
  else
    fail "unknown email type: #{type} for #{first_name} #{last_name}"
  end
  entry.add!("work-email", email)


  # 
  # Addresses and locations:
  # 
  entry.add!("post-number",   f("office location"))
  entry.add!("work-address",  address("business"))
  entry.add!("home-address",  address("home"))
  entry.add!("other-address", address("other"))


  entries.push(entry)
end



## 
## 
## 

entries = entries.sort_by { |x| x.sort_key  } 

#entries.each { |x| puts x.to_s }

puts "---"
entries.each { |x| puts x.old_style }
