## 
## Functions for quoting text for use with Vocola 2.7 and later:
## 

module VocolaQuoting

  # 
  # Quote string so that it may appear as a Vocola word.
  # 
  # Result is always a single token; tries to be as human readable as possible.
  #
  # Does not protect against interpolation or interpretation as a keystroke 
  # sequence.
  # 
  def VocolaQuoting.quote string
    fail "newlines may not appear in string constants"         if string =~ /\n/
    fail "carriage returns may not appear in string constants" if string =~ /\r/

    return string                    if string !~ /[\s()\[\]=|,\"\';\#]|:$|^$/

    return '"' + string + '"'        if string !~ /\"/
    "'" + string.gsub(/'/, "''") + "'"
  end	


  # 
  # Quote string so that it may appear as one or more Vocola actions that 
  # evaluate to string after interpolation and implicit concatenation.
  # 
  # Tries to be as human readable as possible.
  #
  # Does not protect against interpretation as a keystroke sequence.
  # 
  def VocolaQuoting.quote_interpolation string
    # as of Vocola 2.7, no longer need to use multiple actions to handle _'s
    return VocolaQuoting.quote(string.gsub(/\$/, "\\$"))
  end


  # 
  # Translate string of printable characters into a DNS keystroke sequence,
  # which types those characters.
  # 
  def VocolaQuoting.printable_to_keysequence string
    string.gsub(/\{/, "{{}")
  end

  
  # 
  # Translate string of printable characters into one or more Vocola actions 
  # that evaluate to a DNS keystroke sequence after interpolation and
  # implicit concatenation, which types those characters.
  # 
  # Tries to be as human readable as possible.
  # 
  def VocolaQuoting.printable_action printable_string
    VocolaQuoting.quote_interpolation(VocolaQuoting.printable_to_keysequence(string))
  end


  # 
  # Produce Vocola text for the given spoken form; also returns what
  # written form will be inferred if a separate written form is omitted.
  # 
  def VocolaQuoting.quote_spoken spoken
    spoken = spoken.sub(/^\s+/, "").sub(/\s+$/, "")
    spoken = spoken.split(/\s+/)
    fail "spoken string must be nonempty" if spoken.length == 0

    return spoken.collect { |x| VocolaQuoting.quote(x) }.join(" "),
           spoken.join(" ")
  end


  # 
  # Produce Vocola text for an alternative with the given spoken and written 
  # forms.
  # 
  def VocolaQuoting.alternative(spoken, written)
    quoted_spoken, inferred_written = VocolaQuoting.quote_spoken(spoken)

    return quoted_spoken if inferred_written == written 

    return quoted_spoken + " = " + VocolaQuoting.quote_interpolation(written)
  end


  # 
  # Produce Vocola text for a (named) Vocola list definition with the given 
  # list of spoken and written forms.
  #
  # By default, we produce an unnamed inlined list.  If a non-null name is
  # given, we instead produce a named list definition.
  # 
  def VocolaQuoting.list(pairs, name=nil)
    alternatives = pairs.collect { |x,y| VocolaQuoting.alternative(x,y) }

    if name then
      indent = " "*5;
      return "<#{name}> := (\n#{indent}  " +
         alternatives.join("\n#{indent}| ") + "\n);"
    else
      return "(" + alternatives.join(" | ") + ")"
    end
  end
end
