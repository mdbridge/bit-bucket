###
### Code to generate alist-data.el
###

require '~/mail/analysis/folder_splitting'


## 
## Finding all the relevant folders:
## 

NAME = /^.*\/([\-a-z]+)\.([\-a-z]*\.)*([\-a-z]+)$/i

# Return pathnames of Rmail folders for people (first.last)
def folder_paths
  (Dir["/home/mdl/Rmail/*/*"] + Dir["/home/mdl/Rmail/*/*/*"]).
    select { |x| x =~ NAME }.sort
end

# Return [first, last] given a pathname returned by folder_paths
def folder_name pathname
  fail unless pathname =~ NAME
  [$1, $3]
end



## 
## Extracting the name->email address map from a folder (excluding me):
## 

def get_name_map folder_pathname
  results = {}

  Folder.read(folder_pathname).each do |headers, body|
    # hack: allow [MDL] From:'s in body to provide additional information <<<>>>
    headers += body if body =~ /\[MDL\]/i

    headers.scan(/^(?:From|Sender):.*$/i) do |header|
      if header =~ /^(From|Sender):\s*([^<\n]*?)\s+<(.*@.*)>\s*$/i then
        name  = $2
        email = $3
      elsif header =~ /^(From|Sender):\s*([^\s]+@[^\s]+)\s*\(([^@\n]*)\)\s*$/i then
        name  = $3
        email = $2
      elsif header =~ /^(From|Sender):\s*([^\s.]+)\.([^\s.]+)@([^\s]+)\s*$/i then
        name  = "#{$2} #{$3}"
        email = "#{$2}.#{$3}@#{$4}"
      else
        next
      end
      email.downcase!
      name.sub!(/^"(.*)"$/) {$1}          # remove any enclosing quotes


      # ignore shared addresses:
      next if email =~ /[@.]evite.com$/   
      next if email =~ /lj_notify@livejournal.com/
      next if email =~ /@bluemountain.com/
      next if email =~ /@facebookmail.com/
      next if email =~ /@linkedin.com/
      next if email =~ /[@.]tabblo.com$/
      next if email =~ /noreply/

      # ignore bad names:
      next if name =~ /^\s*$/             # skip blank names
      next if name =~ /^[^\s]+@[^\s]+$/
      next if name =~ /mail.*system/i
      next if name =~ /\&/ or name =~ /\band\b/i
      next if name =~ /mail.*server/i

      # ignore me:
      next if name =~ /\bMark\b/i && name =~ /\bLillibridge\b/i

      
      # hack: turn _'s in HP[E] addresses into .'s (wildcards):
      email.gsub!(/_/, ".") if email =~ /@hpe?.com/
      # hack: allow @hp.com or @hpe.com:
      email.gsub!(/hp.com/, "hpe?.com") if email =~ /@hp.com/


      results[name] ||= {}
      results[name][email] = true
    end
  end
  
  results
end

def display_name_map(map, pattern=/impossible54372/)
  map.each_pair do |name, email_addresses|
    print "****" if name =~ pattern
    puts "#{name} -> #{email_addresses.keys.join(', ')}"
  end
  nil
end

# display_name_map(get_name_map("/home/mdl/Rmail/f/melanie.swan"))



## 
## Limiting a name map to the owner of the given folder:
## 

def owner_patterns(first, last)
  # 
  # A pattern based on the owner's first name:
  # 

  # For most first names, only first letter need match:
  first_pattern = /\b#{first[0..0]}/i

  # For a few cases, we need extra letters:
  first_pattern = /\bjan/i             if first == "jan"
  first_pattern = /\bJohn|\bJack/i     if first == "john"

  # For a few cases, alternate names with different starting letters exist:
  first_pattern = /\bpano|\bcipriano/i if first == "pano"
  first_pattern = /\blaura|\bangel/i   if first == "laura" && last == "hall"
  

  # 
  # A pattern based on the owner's last name:
  # 
  last_pattern = /\b#{last}\b/i
  if last =~ /^van/i || last =~ /^von/i then
    last_pattern = /\b#{last[0..2]}\s*#{last[3..-1]}\b/i
  end

  return first_pattern, last_pattern
end


def get_owner_name_map folder_pathname
  first, last = folder_name  folder_pathname
  map         = get_name_map folder_pathname

  first_pattern, last_pattern = owner_patterns(first, last)

  last_map = map.select { |k,v| k =~ last_pattern }
  if last_map.length > 0 then
    return last_map.select { |k,v| k =~ first_pattern }
  else
    # no match on last name, so hope they use just their first name:
    return map.select { |k,v| k =~ /^#{first}$/i }
  end
end

#folder_paths.each { |x| puts "\n#{x}:"; display_name_map(get_owner_name_map(x)) }
#__END__


def display_too_restricted
  folder_paths.each do |path|
    map = get_owner_name_map(path)
    next unless map.length == 0

    puts "\n#{path}:"
    display_name_map(get_name_map(path))

    system("egrep '^(From|Sender):' #{path} | grep -vi facebook")
  end
end

#display_too_restricted
#__END__


## 
## Given a name map, return the unique list of email addresses mapped to:
## 

def target_addresses map
  result = []
  seen = Hash.new

  map.each_value do |emails|
    emails.each_key do |email|
      result.push(email) if !seen[email]
      seen[email] = true
    end
  end

  result.sort
end

#folder_paths.each {|x| puts "\n#{x}:\n#{target_addresses(get_owner_name_map(x)).join(', ')}"}
#__END__


## 
## Given a folder pathname, return a list of email addresses believed
## to belong to that folder's owner:
## 

def owner_addresses(folder)
  email_addresses =  target_addresses(get_owner_name_map(folder))

  # Remove obsolete email addresses that it should be impossible to
  # get email from anymore:
  email_addresses.reject { |x| x =~ /compaq.com$|dec.com$|digital.com$/i }
end

#folder_paths.each {|x| puts "\n#{x}:\n#{owner_addresses(x).join(', ')}"}


## 
## Combine folder email information with folder information:
## 

def email_folder_pairs 
  results = []

  folder_paths.each do |folder|
    email_addresses = owner_addresses(folder)

    email_addresses.each { |x| results << [x, folder.sub("/home/mdl/","~/")] }
  end

  results
end

#p email_folder_pairs
#__END__



### 
### Main routine:
### 

def put_pairs(text)
  text.split(/\n/).each do |line|
    if line =~ /^\s*$/ then
      puts
      next
    end
    
    email, folder = line.split(/,\s*/)
    put_pair(email, folder)
  end
end

def put_pair(email, folder)
  maximum = 37
  quoted_email = left_justify('"' + email + '"', maximum)
  puts "  (#{quoted_email} . \"#{folder}\")"
end
def left_justify(text, size)
  if text.length > size then
      text
  else
    text + " "*(size-text.length)
  end
end


puts <<HEADER
;;
;; WARNING: This file is automatically generated by generate_alist.rb; do not
;; modify
;;

(defvar rmail-output-file-alist)  ; suppress warning about free variables

(setq rmail-output-file-alist \'(
HEADER


put_pairs(<<PRIORITY)
hpl-pa@hpl.hp.com,                   ~/Rmail/h/hp_labs
hpl-ww@hpl.hp.com,                   ~/Rmail/h/hp_labs
hpl-pa@labs.hpecorp.net,             ~/Rmail/h/hp_labs
hpl-ww@labs.hpecorp.net,             ~/Rmail/h/hp_labs

simpl@groups.hp.com,                 ~/Rmail/h/simpl
simpl-interest@groups.hp.com,        ~/Rmail/h/simpl_interest

david.falkinder@hpe?.com,            ~/Rmail/h/tube
graham.perry@hpe?.com,               ~/Rmail/h/tube
john.butt@hpe?.com,                  ~/Rmail/h/tube
mark.watkins@hpe?.com,               ~/Rmail/h/tube

benefitsus@hpe?.com,                 ~/Rmail/h/benefits
usbenefits@hpe?.com,                 ~/Rmail/h/benefits
us.payroll@hpe?.com,                 ~/Rmail/h/benefits

kira.linsmeier@hpe?.com,             ~/Rmail/h/health
gloria@yogawithgloria.com,           ~/Rmail/h/health
stephanie.lovejoy@hp.com,            ~/Rmail/h/health


types-announce@lists.seas.upenn.edu, ~/Rmail/b/cs
types-list@lists.seas.upenn.edu,     ~/Rmail/b/cs
^Subject: \\\\[TYPES/announce],      ~/Rmail/b/cs
^Subject:.*call for papers,          ~/Rmail/b/cs
^Subject:.*call for position papers, ~/Rmail/b/cs

messages-noreply@linkedin.com,       ~/Rmail/b/LinkedIn
updates@linkedin.com,                ~/Rmail/b/LinkedIn


sigmaxi@smartbrief.com,              ~/Rmail/m/sigma_xi

announce@direct.mercurynews.com,     ~/Rmail/p/companies
newsletter@gog.com,                  ~/Rmail/p/companies
noreply@onlymytoyota.com,            ~/Rmail/p/companies
noreply@youtube.com,                 ~/Rmail/p/companies
snapfish@.*,                         ~/Rmail/p/companies
sourceforge@www.elabs10.com,         ~/Rmail/p/companies
Vanguard@eonline.evanguard.com,      ~/Rmail/p/companies

news-admin@kurzweilai.net,           ~/Rmail/p/kurzweil
newsletter@kurzweilai.net,           ~/Rmail/p/kurzweil

editorial@e.mckinseyquarterly.com,   ~/Rmail/p/mckinsey


VoiceCoder@yahoogroups.com,          ~/Rmail/p/voice-coder

fnw@worldware.com,                   ~/Rmail/d/fnw
PRIORITY

puts
puts

#__END__


# only send to these if no other normal folders match:
cc_people  = /laura.lillibridge|kave.eshghi|alistair/i

pairs = email_folder_pairs()

low_pairs  = pairs.select { |e,f| f =~ cc_people }
high_pairs = pairs.reject { |e,f| f =~ cc_people }

high_pairs.each do |email, folder|
  email = email.gsub(/([+])/) { '\\\\' + $1 }
  put_pair(email, folder)
end
puts
puts
low_pairs.each do |email, folder|
  email = email.gsub(/([+])/) { '\\\\' + $1 }
  put_pair(email, folder)
end


put_pairs(<<LOW)

.,                                ~/Rmail/unknown
LOW

puts <<'TRAILER'
))
TRAILER
