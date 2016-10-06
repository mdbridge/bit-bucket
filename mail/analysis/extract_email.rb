### 
### Attempt to pull out relevant email text for training corpus for DNS
### 
### 
### Roughly, recent non-MIME email from me without quoted text, long signatures
### 

require '~/mail/analysis/folder_splitting'

require "date"


def extract(folder, days)
  folder.each do |header, body|
    process(header, body, days)
  end
end


def process(headers, body, days)
  # this test isn't 100% safe:
  if headers =~ /^Content-Type:.*(\n\s.*)*boundary/i or
      headers =~ %r{^Content-Type: (text/html|message/rfc822)}i  or
      headers =~ /^Content-Disposition: attachment/i  or
      body   =~ /^Content-Type:.*(\n\s.*)*boundary/i then
    #$stderr.puts "MIME!"
    return
  end

  if headers =~ /^Content-Transfer-Encoding: quoted-printable/i then
    #$stderr.puts "Quoted Printable!"
    return  # <<<>>>
  end


  if headers    =~ /^Mail-from: from ([^\s]*) /i then
    sender = $1
  elsif headers =~ /^From: .*<([^\s]*)>/ then
    sender = $1
  elsif headers =~ /^From: ([^\s'"(]*@[^\s]*)[ \n]/ then
    sender = $1
  elsif headers =~ /^From: ([a-z]+) \([a-zA-Z ]+\)/ then
    # hack for some really old email (rescued_DJ_email/*):
    sender = $1
  elsif headers =~ /^Return-Path: <([^>]*)>$/ then
    # this tends to have crap in front of / modify email address now:
    sender = $1
  else
    #$stderr.puts headers
    #fail "badly formatted message: no sender"
    return
  end
  #puts "from: #{sender}"

  return unless sender =~ /^mark.lillibridge/i or sender =~ /^mdl@/


  if !(headers =~ /^Date:  *(.*)$/) then
    $stderr.puts headers
    fail "badly formatted message: no date"
  else
    date = Date.parse($1)
  end
  #puts "sent: #{date.to_s()}  (#{(Date.today() - date)} days ago)"

  return unless (Date.today() - date) < days


  body.gsub!(/Start of forwarded message.*/im, "")
  body.gsub!(/===== Forwarded message.*/im, "")
  body.gsub!(/----+ *Original Message.*/im, "")
  body.gsub!(/Community email addresses:.*/m, "")


  # uncomment to sample long messages:
#  return unless body.gsub(/^[^>].*$/,"") =~ /\n{30}/   # uncomment to sample long messages:
#  return unless rand(145)<30


  body.gsub!(/^.*writes:\n/, "")
  body.gsub!(/^>.*\n/, "")

  body.gsub!(/^- Mark\n/, "")
  body.gsub!(/^- Thanks,\n  Mark\n/, "")
  body.gsub!(/^- Sorry,\n  Mark\n/, "")

#  puts "\f"
#  puts "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
  puts "\n\n#{body}"
end



## 
## Main routine:
## 

fail "usage: extract_email.rb <days> <email folder file>+" if ARGV.length < 2

days = ARGV[0].to_i

ARGV[1..-1].each do |filename|
  next if filename =~ /~$/

  if File.file?(filename) then
    modified = Date.parse(File.mtime(filename).to_s)
    next unless (Date.today() - modified) < days

    #$stderr.puts "  scanning: #{filename}"
    begin
      extract(Folder.read(filename), days)
    rescue 
      $stderr.puts "**** #{filename} caused an exception:"
      raise
    end
  end
end
