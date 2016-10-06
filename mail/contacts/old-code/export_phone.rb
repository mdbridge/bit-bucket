##
## Code to extract rolodex entries for Verizon's backup assistant plus:
##

require './rolodex'


#
# read in rolodex:
#
R = Rolodex.load(File.open("addresses.rol"))
#R = Rolodex.load(File.open("HP.rol"))

def first(possible_list)
  return possible_list unless possible_list.respond_to?(:to_ary)
  possible_list[0]
end


#
# Output selected fields to a .csv file suitable for import into
# backup assistant plus:
#
count = 0
File.open("to_phone.csv", "w") do |output|
  # columns from a backup assistant plus export file:
  output.puts '"Prefix","First Name","Middle Name","Last Name","Suffix","Company Name","Company Department","Job Title","Business 1 Street Address","Business 1 City/Region","Business 1 State","Business 1 Zip Code","Business 1 Country","Business 2 Street Address","Business 2 City/Region","Business 2 State","Business 2 Zip Code","Business 2 Country","Business 3 Street Address","Business 3 City/Region","Business 3 State","Business 3 Zip Code","Business 3 Country","Home 1 Street Address","Home 1 City/Region","Home 1 State","Home 1 Zip Code","Home 1 Country","Home 2 Street Address","Home 2 City/Region","Home 2 State","Home 2 Zip Code","Home 2 Country","Personal 1 Street Address","Personal 1 City/Region","Personal 1 State","Personal 1 Zip Code","Personal 1 Country","Personal 2 Street Address","Personal 2 City/Region","Personal 2 State","Personal 2 Zip Code","Personal 2 Country","Personal 3 Street Address","Personal 3 City/Region","Personal 3 State","Personal 3 Zip Code","Personal 3 Country","Other Street Address","Other City/Region","Other State","Other Zip Code","Other Country","Assistant","Business Fax 1","Business Fax 2","Business Fax 3","Business Fax 4","Business Fax 5","Work Number 1","Work Number 2","Work Number 3","Work Number 4","Work Number 5","Callback","Car Phone","Company Main Phone","Home Fax","Home Number 1","Home Number 2","Home Number 3","Home Number 4","Home Number 5","Mobile Number 1","Mobile Number 2","Mobile Number 3","Mobile Number 4","Mobile Number 5","Other Fax","Other Number 1","Other Number 2","Other Number 3","Other Number 4","Other Number 5","Pager 1","Pager 2","Pager 3","Pager 4","Pager 5","Main Telephone","Radio","TTYDD","Telex","Videophone","Voiphone","Primary Fax","School","Anniversary","Assistant Name","Date of Birth","Children","Email Personal","Email Business","Email Other","Gender","Notes","Profession","Spouse","Groups / MobileGroups","Photo","Nickname","Relationship","Time Zone","Marital Status","ICE","Photo Type","IM AIM","IM Yahoo","IM MSN","IM Google","IM ICQ","IM Skype","IM Jabber","IM Other","Web Page Business 1","Web Page Business 2","Web Page Business 3","Web Page Personal","Web Page Other"' + "\r"

  R.each do |entry|
    name   = entry["name"]
    if name !~ /,/ then
      name = "," + name

      livejournal = entry["livejournal"]
      name = "(LJ #{livejournal})" + name if livejournal
    end
    last, first = name.split(/,\s*/)

    email      = first(entry["email"])
    home_email = first(entry["home-email"])
    work_email = first(entry["work-email"])

    cell       = entry["cell"] || ""
    home       = entry["home"] || ""
    work       = entry["work"] || ""
    phone      = entry["phone"] || ""

    groups     = entry["group"] || []

    
    if groups.include?("not_phone") then
      puts "Skipping #{name} due to not_phone marker..." 
      next
    end

    # Skip entries without a phone number:
    if cell=="" and home=="" and work=="" and phone=="" then
      #puts "Skipping #{name} due to no phone..." 
      next
    end

    if groups.include?("5E") and not groups.include?("wedding") then
      #puts "Skipping #{name} due to 5E but not wedding..." 
      next
    end


    home_email = email unless home_email   # device only keeps home & work email

    output.puts ",#{first},,#{last}," +
      ",,,,,,,,,,,,,,,,,,," +                  # Company info
      ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,," + # home/personal addresses
      "#{work},,,,,,,,,#{home},,,,,#{cell},,,,,,#{phone},,,,,,,,,,,,,,,,,,,,,," +
      "#{home_email},#{work_email},#{email},"
    count += 1
  end
end

puts "#{count} contacts generated..."
