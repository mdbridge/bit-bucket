def lookup_file(spoken, extension)
  load_files() unless $files

  words   = spoken.gsub(/[^\w\s]/, "").split(/\s+/)
  pattern = words.collect { |x| x[0] + x[1..-1].gsub(/./, "\\&?") }.join("")
  regexp  = /^#{pattern}$/i

  matching = $files.select { |s,e,p| s.gsub(/_/, "") =~ regexp and e =~ extension }
  matching = matching.sort_by { |s,e,p| -s.length  }

  matching[0]
end

$files = nil
def load_files
  $files = File.read("/home/#{ENV['USER']}/Tmp/files").split(/\n/).
                collect { |x| x =~ %r_^ (.*)/([^/]*?)(\.[^/]*)?$_; 
                              [$2, $3||"", "#$1/#$2#$3", "#$2#$3"] }
end


if ARGV.length != 3 then
  fail "usage: <spoken form> <extension pattern> [0-3]"
end

result = lookup_file(ARGV[0], /#{ARGV[1]}/)
print result[ARGV[2].to_i] if result
