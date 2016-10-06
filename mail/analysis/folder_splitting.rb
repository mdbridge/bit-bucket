###
### Code to open and parse an email folder file into individual
### messages and/or headers, body pairs.
### 
### Currently only understands the formats BABYL and mbox.
### 
### Does not process character sets correctly; everything returned is
### 8-bit binary.
### 

# Note: this code requires Ruby 1.9


class Folder

  def Folder.read(filename)
    text = File.open(filename, "rb:binary") { |x| x.read() }
    Folder.make(text)
  end

  def Folder.make(text)
    case text
    when /^BABYL OPTIONS:/
      BabylFolder.new(text)
      
    when /^From /, /\A\z/
      MboxFolder.new(text)
      
    else
      fail "unknown mail folder kind"
    end
  end


  # subclass defined methods: length, [i]

  def to_s
    "<an email folder with #{length} messages>"
  end

  def each_message
    length().times { |x| yield self[x] }
  end


  # returns normal headers, body[, filtered headers]:
  def divide(message)
    if message =~ /\A(.*?\n)\n(.*)/m then
      return $1, $2
    else
      return message, ""
    end
  end

  def each
    each_message do |message|
      yield(*divide(message))   # 2-3 values
    end
  end


  def test
    puts "\n\n"
    puts self.class
    
    puts self[-1].inspect if BabylFolder === self
    each_message do |x|
      puts((x.split(/\n/)[0..4].join("\n") + ' ... ' + x[-100..-1]).inspect)
    end
  end

  def test2
    puts "\n\n"
    puts self.class
    
    each do |h,b,f|
      puts h
      if f then
        puts "-----------------------------------------------\n" + f
      end
      puts   "===============================================\n" + b
      puts "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    end
  end
end



class BabylFolder < Folder
  def initialize(text)
    fail "not a BABYL file" unless text =~ /^BABYL OPTIONS:/

    @sections = text.split(/\n\x1f\x0c/)
    @sections[-1].sub!(/\x1f\s*\Z/, "")
  end


  def length
    @sections.length - 1
  end

  # -1 == BABYL option section
  def [](i)
    section = @sections[i+1] + "\n"
    return section if i == -1

    if section[0] != "\n"
      fail "bad BABYL message start: #{section[0..200].inspect}"
    end
    
    section[1..-1]
  end


  def divide(message)
    case message[0]
    when '0'
      header, body = super
      if header =~ /\A(0,.*$\n(Summary-line:.*$\n)?)\*\*\* EOOH \*\*\*\n/
        return $1+$', body, nil
      else
        fail "bad BABYL message format: #{message[0..200].inspect}"
      end
      
    when '1'
      if message =~ /\A(.*?)\n\*\*\* EOOH \*\*\*\n(.*)/m then
        header = $1
        filtered, body = super($2)
        return header, body, filtered
      else
        fail "bad BABYL message format: #{message[0..200].inspect}"
      end
    
    else
      fail "bad BABYL message start: #{message[0..200].inspect}"
    end
  end
end



class MboxFolder < Folder
  def initialize(text)
    @sections = []
    return if text == ""

    fail "not a mbox file" unless text =~ /\AFrom /

    @sections = text.split(/^From /)[1..-1]
  end


  def length
    @sections.length
  end

  def [](i)
    message = @sections[i]
    fail "bad mbox format (error at end of message #{i})" if message[-1]!="\n"

    "From " + message[0...-1]
  end


  def divide(message)
    header, body = super

    body.gsub!(/^>([>]*From )/, "\\1")
    #body.gsub!(/^>From /, "From ")

    return header, body
  end
end



#Folder.read("empty").test()
#Folder.read("empty-babyl").test()
#Folder.read("/home/mdl/Rmail/RMAIL").test()
#Folder.read("/home/mdl/mail/incoming").test()

#Folder.read("/home/mdl/mail/incoming").test2()
#Folder.read("/home/mdl/Rmail/RMAIL").test2()
