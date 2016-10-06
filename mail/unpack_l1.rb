### 
### Unpack message contained in ~/Tmp/l1 to ~/Tmp/export
### 

def failstop_system(command)
  system(command)
  fail "failed command: #{command}: #$?" if $? && $?.exitstatus != 0
end

system("rm -rf ~/Tmp/export")
if $? && $?.exitstatus != 0
  puts "warning: relocating undeletable export directory"
  system("mv ~/Tmp/export ~/Tmp/k_#{rand(100000)}")
end

failstop_system("rm -rf ~/Tmp/export ~/Tmp/l1.html ~/Tmp/l1.txt")
failstop_system("mkdir ~/Tmp/export")
failstop_system("ln -s ~/Tmp/l1 ~/Tmp/l1.html")
failstop_system("ln -s ~/Tmp/l1 ~/Tmp/l1.txt")
failstop_system("ln -s ~/Tmp/l1 ~/Tmp/export/ZZZ_l1")


  # installed version of munpack >= 1.6.4 else extracts filenames incorrectly:
failstop_system("cd ~/Tmp/export; munpack1.6 -t ZZZ_l1 > ZZZ_contents_")


no_parts = false
text     = nil
text1    = nil
html     = nil
calendar = nil
File.open("/home/mdl/Tmp/export/ZZZ_contents_") do |contents|
  contents.each do |line|
    action = ""

    if line =~ /^(part\d+) \(text\/([a-z]*)\)/i then
      case $2.downcase()
      when "plain"
        text1 = text
        text  = "ZZX_#$1.txt"
        failstop_system("mv ~/Tmp/export/#$1 ~/Tmp/export/#{text}")
        action = " -> #{text}"
      when "html"
        html = "ZZX_#$1.html"
        failstop_system("mv ~/Tmp/export/#$1 ~/Tmp/export/#{html}")
        action = "  -> #{html}"
      when "calendar"
        calendar = "ZZX_#$1.ics"
        failstop_system("mv ~/Tmp/export/#$1 ~/Tmp/export/#{calendar}")
        action = " -> #{calendar}"
      end
    end

    if line =~ /^(.*.ics) \(text\/calendar\)/i then
      calendar = $1
      action = " [meeting]"
    elsif line =~ /^(.*.vcs) \(text\/plain\)/i then
      calendar = $1
      action = " [meeting]"
    end

    if line =~ /Did not find anything to unpack/ then
      no_parts = true
    end
    
    puts line.chomp() + action
  end
end


if no_parts then
  message = File.read("/home/mdl/Tmp/l1", encoding:"binary")
  if message =~ /\A((?:[^\n]+\n)+)\n(.*)\z/m then
    header, body = $1, $2
    if header =~ /^mime-version:/i and header =~ %r<^content-type: text/html>i then
      html = "inline_HTML.html"
      File.open("/home/mdl/Tmp/export/#{html}", "w:binary") { |x| x.print body }
      puts "<message body> ->  #{html}"
    end
  end
end



text_summary = "/home/mdl/Tmp/export/ZZB_text_summary.txt"
failstop_system("cat ~/Tmp/export/ZZZ_contents_ > #{text_summary}")
File.open(text_summary, "a") do |output|
  output.puts "======================================="
end
if text then
  if text1 then
    failstop_system("cat ~/Tmp/export/#{text1} >> #{text_summary}")
    failstop_system("echo '' >> #{text_summary}")
    failstop_system("echo '========================================' >> #{text_summary}")
  end
  failstop_system("cat ~/Tmp/export/#{text} >> #{text_summary}")
else
  failstop_system("cat ~/Tmp/export/ZZZ_l1  >> #{text_summary}")
end


def adjust_HTML(source)
  target = "~/Tmp/export/ZZA_html.html"

  # failstop_system("ln -s #{source} #{target}")

  failstop_system("sed 's/[\" ]cid:\\([^\"]*\\)@[0-9a-fA-F.]*\"/\\1/g' " +
                  "#{source} > #{target}")
end

if html then
  adjust_HTML("~/Tmp/export/#{html}")
else
  adjust_HTML("~/Tmp/export/ZZZ_l1")
end


if calendar then
  failstop_system("cat ~/Tmp/export/#{calendar} >> ~/Tmp/export/_meeting.ics")
end


if File.exist?("/home/mdl/Tmp/export/webpage.zip.gz") then
  puts "ungzipping webpage.zip.gz..."
  failstop_system("(cd ~/Tmp/export; gunzip webpage.zip.gz) > /dev/null") 
end
if File.exist?("/home/mdl/Tmp/export/webpage.zip") then
  puts "unzipping webpage.zip..."
  failstop_system("(cd ~/Tmp/export; unzip webpage.zip) > /dev/null") 
end
