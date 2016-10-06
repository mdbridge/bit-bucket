## 
## Backup filename (not path) ARGV[0] into local directory backups
## 

directory = "backups"

fail unless ARGV.size == 1
target = ARGV[0]


def last_version(prefix)
  version = 1

  while File.exist?("#{prefix}#{version}") do
    version += 1
  end

  return version - 1
end


prefix = "#{directory}/#{target}__"
last_version = last_version(prefix)

last = "#{prefix}#{last_version}"
new  = "#{prefix}#{last_version+1}"


def md5(file)
  `md5sum #{file}`.split(/ /)[0]
end

if File.exist?(last) && md5(last) == md5(target) then
  exit(0)
end

puts   "cp #{target} #{new}"
system("cp #{target} #{new}")
