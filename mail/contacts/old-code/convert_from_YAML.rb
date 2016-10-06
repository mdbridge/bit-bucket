require './code/rolodex.rb'

require 'yaml'
require './patch'
# backward compatibility with 1.9.3 for now:
YAML::ENGINE.yamler = "syck"

$all = {}

class Rolodex 
#  def map
#    result = Rolodex.new
#    keys.each do |name|
#      output = yield(self[name])
#      result.add(output) if output
#    end
#    result
#  end
#
#  def reject
#    result = Rolodex.new
#    keys.each do |name|
#      entry = self[name]
#      result[entry.key] = entry if !yield(entry)
#    end
#    result
#  end

  def Rolodex.load2 source
    result = Rolodex.new
    YAML.load(source.read).each_pair do |k,v|
      fields = {}
      v.to_a.each do |field, value|
        $all[field]= true
        next if field == "name"

        values = []
        if field == "children" then
          # special conversion case:
          field = "child"
          if value =~ /;/ then
            values = value.split(/\s*;\s*/)
          else
            values = value.split(/\s*,\s*/)
          end
        else
          case value
          when Array
            value.each { |x| values += [x] }
          else
            if value =~ /\n/ then
              values = value.split(/\n/)
            else
              values = [value]
            end
          end
        end

        #puts values.inspect
        fields[field] = values
      end

      result.add RolodexEntry.new(k, fields)
    end
    #puts result.inspect

    result
  end

end


R = Rolodex.load2(File.open("addresses"))

R.dump(File.open("addresses.rol", "w"))
