class RolodexEntry

  def initialize
    @fields = []  # each of form [key, value]
  end

  def add!(key, value)
    return unless value
    @fields.push([key, value])
  end

  def to_s
    @fields.collect do |key, value|
      key + ": " + value.gsub(/\n+\z/, "").gsub(/\n/, "\n  ")
    end.join("\n") + "\n%%\n"
  end

  def old_style
    first = @fields[0]
    fail unless first[0]=="reversed-name"
    
    text = first[1] + ":\n"
    @fields[1..-1].collect do |key, value|
      value = value.gsub(/\n+\z/, "").gsub(/\A\n+/, "")
      if value !~ /\n/ then
        value = "\"#{value}\"" if value =~ /:/
        text += "  " + key + ": " + value + "\n"
      else
        text += "  " + key + ": |\n    " + value.gsub(/\n/, "\n    ")+"\n\n"
      end
    end
    text
  end

  def sort_key
    first = @fields[0]
    fail unless first[0]=="reversed-name"

    first[1].downcase
  end

end
