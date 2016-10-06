require "./rolodex"
require 'yaml'

# returns list of rows (including header), each of which is a list of
# the field values; note that values may contain \n
def get_tab_separated file
  raw = file.read              # read as a single string

  # convert from MS-DOS format if needed
  unix = raw.gsub(/\r/, "").chomp

  quotation = '"[^"]*"'   # if change this, change unquote expression below

#  rows = unix.gsub!(/#{quotation}|\n/) {|m| m=~/"/ ? m : "\0"}.split("\0")
  rows = unix.scan(/(?:#{quotation}|.)+/)

  rows.map do |row|
    row.gsub!(/#{quotation}|\t/) {|m| m=~/"/ ? m[1...-1] : "\0"}.split("\0")
  end
end

# similar to get_tab_separated, but consumes header line and returns rows
# as hashes from field names to their non-empty values
def get_tab_hashes file
  rows = get_tab_separated file

  header = rows.shift
  field_to_index = {}
  index_to_field = {}
  header.each_index do |i|
    field_to_index[header[i]] = i
    index_to_field[i] = header[i]
  end

  hashes = rows.map do |row|
    result = {}
    row.each_index do |i|
      result[index_to_field[i]] = row[i] if row[i] != ""
    end
    result
  end
  return hashes, field_to_index, index_to_field
end



def outlook_key outlook_entry
  first_name  = outlook_entry["First Name"]
  middle_name = outlook_entry["Middle Name"]
  last_name   = outlook_entry["Last Name"]

  first_name += " #{middle_name}" if middle_name && middle_name != ""

  return last_name + ", " + first_name if first_name && last_name
  return first_name if first_name

  nil
end

def outlook_email out_entry
  return out_entry["E-mail Address"] if out_entry["E-mail Type"] == "SMTP"

  if out_entry["E-mail Type"] == "EX"
    out_entry["E-mail Display Name"].scan(/\(([^)]*@[^)]*)\)/){|x| return $1}
  end

  nil
end

def to_rolodex_entry outlook_entry
  entry = {}

  name = outlook_key outlook_entry
  fail "Outlook entry without a name" if !name
  entry["name"] = name

  if spoken = outlook_entry["User 1"] then
    entry["spoken"] = spoken
  end

  email = outlook_email outlook_entry
  entry["work-email"] = email if email
  # $stderr.puts "can't find email for entry #{name}" if !email

  if phone = outlook_entry["Business Phone"] then
    entry["work"] = phone
  end
#puts outlook_entry.inspect

  entry["source"] = ["outlook"]
  entry
end



rows,f,i = get_tab_hashes(File.open("outlook.tsv", "rb:binary"))
#rows,f,i = get_tab_hashes($<)

rolodex = Rolodex.new
rows.map { |x| rolodex.add(to_rolodex_entry(x)) }
rolodex.dump
