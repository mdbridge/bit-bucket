require 'digest/md5'

# should we try and make most grammars global?
$make_global = false
$make_global = true if `hostname` =~ /foil/
$make_global = true

$location = "work"
$location = "home" if `hostname` =~ /foil/


def translate_file(filename)
  basename = File.basename(filename)
  contents = IO.read(filename).gsub(/\r/, "") # ensure UNIX encoding

  translate_contents(basename, contents)
end


def switch_app(basename, contents, new_app, include_file=nil, extra=nil)
  new_basename = basename.sub(/^[^._]*/, new_app)
  new_contents = <<TOP
#### 
#### AUTOMATICALLY GENERATED from #{basename}
#### 
                                                
TOP

  new_contents += "include \"#{include_file}.vch\";\n\n" if include_file
  new_contents += extra + "\n\n"                         if extra
  new_contents += contents

  translate_contents(new_basename, new_contents)
end


def translate_contents(basename, contents)
  if basename =~ /^(.*)@@(.*)\.(.*)$/ then
    if $2 == $location
      basename = "#$1.#$3"
    else
      return
    end
  end

  if basename =~ /.vch$/ then
    output(contents, basename)
    return
  elsif basename !~ /.vcl$/ then
    $stderr.puts "Error: non-Vocola file: #{basename}"
    return
  end
    

  application = basename[/^[^._]*/]
  case application

    #
    # The fake application "gnu" provides commands good for both x and 
    # emacs; includes the appropriate locale_*.vch file.
    #
  when "gnu"
    if $make_global and basename !~ /short|range/ then
      switch_app(basename, contents, "__gnu_", "locale_mixed")
    else
      switch_app(basename, contents, "emacs_", "locale_PC")
      switch_app(basename, contents, "x_",     "locale_Unix")
    end

    #
    # The fake application "UNIX" provides commands good for x, putty,
    # and mintty; includes the appropriate locale_*.vch file and
    # Empty() definition.  
    #
    # This is mainly intended for tcsh shell commands.
    #
  when "UNIX"
    if $make_global and basename !~ /short|range/ then
      empty_definition = 'Empty() := If(Window.Match("Cygwin"), {ctrl+e},' +
                                       'If(Window.Match(emacs), {f5}, ""));'
      switch_app(basename, contents, "__UNIX_", "locale_Unix", empty_definition)
    else
      switch_app(basename, contents, "x_", "locale_Unix",
                 'Empty() := If(Window.Match("Cygwin"), {ctrl+e}, {f5});')
      # putty is usually connected to a Linux box running a Unix shell:
      switch_app(basename, contents, "putty_", "locale_Unix",
                 "Empty() := {ctrl+e};")
      switch_app(basename, contents, "mintty_", "locale_Unix",
                 "Empty() := {ctrl+e};")  # <<<>>>
    end
    
    #
    # The fake application "x" provides commands good for all X
    # servers I use.
    #
  when "x"
    if $make_global and basename !~ /short|range/ then
      switch_app(basename, contents, "__x_")
    else
      switch_app(basename, contents, "xwin_")
      switch_app(basename, contents, "xpra_cmd_")
      switch_app(basename, contents, "xpra_")
      #switch_app(basename, contents, "rx_")
      #switch_app(basename, contents, "xming_")
      #switch_app(basename, contents, "nxwin_")
    end


    #
    # The fake application "browser" provides commands good for 
    # Internet Explorer, Firefox, and Chrome.
    #
  when "browser"
    switch_app(basename, contents, "Firefox_",  "Firefox")
    switch_app(basename, contents, "iexplore_", "iexplore")
    switch_app(basename, contents, "chrome_", "chrome")


    #
    # Temporary hack to deal with Reflection bug:
    # 
  when "rx"
    contents = contents.gsub(/\{home/, "{f5}{home").
                        gsub(/\{end/,  "{f5}{end").
                        gsub(/\{down/, "{f5}{down").
                        gsub(/\{up/,   "{f5}{up")
    output(contents, basename)


  else
    output(contents, basename)
  end
end


def output(contents, basename)
  target = "/home/#{ENV['USER']}/voice/my_commands/to_Vocola/#{basename}"

  # kludge to handle most include dependencies (not environment
  # variables, containing spaces)
  contents.gsub!(/^include *(\"?)([^\" ]*)\1[ ]*;[ \t]*(\#.*)?$/) do |info|
    begin
      include_file = IO.read("/home/#{ENV['USER']}/voice/my_commands/to_Vocola/#{$2}")
      info.sub(";", "; \# #{Digest::MD5.hexdigest(include_file)} ")
    rescue
      info
    end
  end

  old_contents = nil
  old_contents = IO.read(target) unless !File.exist?(target)

  return unless contents != old_contents

  puts ">>> UPDATING #{basename}..."
  File.open(target, "w") { |x| x.print contents }
  $changed = true
end


$changed = true
while $changed do
  $changed = false
  ARGV.each { |x| translate_file(x) }
end
