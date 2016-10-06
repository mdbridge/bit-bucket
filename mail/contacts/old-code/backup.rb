directory = "backups"

# hard code target for now: <<<>>>
target = "addresses"


def last_version(prefix)
  version = 1

  while File.exists?("#{prefix}#{version}") do
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

if File.exists?(last) && md5(last) == md5(target) then
  exit(0)
end

puts "cp #{target} #{new}"
system("cp #{target} #{new}")
