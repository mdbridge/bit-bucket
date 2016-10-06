### 
### Convert exported contacts from Outlook to a Mark-style Rolodex
### 
### Uses LDAP when necessary to get work email addresses.
### 
### The following extra fields are used in contact information:  [depreciated]
### 
###   User 1: alternative spoken name for nickname (default is first name)
###   User 2: if present, additional nickname (nickname-2)
### 

require './code/rolodex'


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

#rows = read_Outlook_tabs("outlook.txt")
rows = read_Outlook_CSV("outlook.csv")

def enumerate_contacts rows
  field_names = rows.shift.map(&:downcase)
  rows.each do |row|
    result = {}
    row.zip(field_names).each { |v,f| result[f] = v if v!="" }
    yield result
  end
end

def address(contact, prefix)
  return unless contact[prefix + " street"]

  ["street", "street 2", "street 3", "city", "state", "postal code", 
   "country/region"].collect do |item|
    contact[prefix + " " + item]
  end.compact.join("\n").gsub("\\n","\n") + "\n"
end

def combine_names names
  names.compact.select { |x| x!="" }.join(" ")
end



## 
## Convert each Outlook entry to a RolodexEntry:
## 

R = Rolodex.new


def add_field name, value
  return unless value
  values = value.split(/\n/)
  $new_fields.push [name, values]
end

enumerate_contacts(rows) do |c|
  #puts c.inspect
  $new_fields = []
  
  # 
  # Names:
  # 
  title       = c["title"]
  first_name  = c["first name"]
  middle_name = c["middle name"]
  last_name   = c["last name"]
  suffix      = c["suffix"]       # usually used for disambiguation

  # hack to fix middle field often not being properly populated for
  # real people:
  if c["business phone"] and c["e-mail type"] and not middle_name then
    if first_name =~ /^(\w+) (\w.*)$/ then
      first_name, middle_name = $1, $2
    end
  end

  #name       = combine_names([first_name, last_name])
  first_part = combine_names([title, first_name, middle_name])
  last_part  = combine_names([last_name])
  fail unless first_part + last_part != ""

  full      = combine_names([first_part, last_part, suffix])

  reversed  = last_part
  reversed += ", " if reversed != "" and first_name != ""
  reversed += first_name
  key = reversed

  add_field "full",       full
  add_field "nickname",   c["user 1"] || first_name
  add_field "nickname-2", c["user 2"]


  # 
  # Position information:
  # 
  add_field "job-title",  c["job title"]
  add_field "department", c["department"]
  add_field "company",    c["company"]
  add_field "assistant",  c["assistant's name"]


  # 
  # Phone numbers:
  # 
  add_field "cell",          c["mobile phone"]
  add_field "work",          c["business phone"]
  add_field "work2",         c["business phone 2"]
  add_field "home",          c["home phone"]
  add_field "home2",         c["home phone 2"]
  add_field "phone",         c["other phone"]
  add_field "primary-phone", c["primary phone"]  # not used by any current contacts


  # 
  # Email:
  # 
  type = c["e-mail type"]
  case type
  when nil       # No email listed at all...
    email = nil

  when "SMTP"
    email = c["e-mail address"]
    
  when "EX"
    display = c["e-mail display name"]
    # ignoring display for now to make sure see leaving employees: <<<>>>
    if false and display =~ /\(([^)]*@.*)\)/ then
      email = $1
    else
        # first look up by first, last name:
      email = `code/lookupEmailLDAP.pl 'cn=#{first_name}*#{last_name}'`.chomp
      if email =~ /^FAIL/ then
        problem = "  #{email}"
        # then try by username:
        email = `code/lookupEmailLDAP.pl 'ntUserDomainId=*:#{c["account"]}'`.chomp
      end
      if email =~ /^FAIL/ then
        $stderr.puts "ERROR: unable to find email for #{first_name} #{last_name}:"
        $stderr.puts problem, "  #{email}"
        email = "unknown"
        fail
      end
    end
    
  else
    fail "unknown email type: #{type} for #{first_name} #{last_name}"
  end
  add_field "work-email", email


  # 
  # Addresses and locations:
  # 
  add_field "post-number",   c["office location"]
  add_field "work-address",  address(c, "business")
  add_field "home-address",  address(c, "home")
  add_field "other-address", address(c, "other")


  R.add(RolodexEntry.new(key, $new_fields))
end



## 
## 
## 

R.dump
