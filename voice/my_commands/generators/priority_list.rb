### 
### Building Vocola lists from possibly conflicting entries (e.g.,
### "equivalent" spoken forms"), resolving ambiguities via priorities,
### with ties broken by insertion order (earliest wins).
### 

require '~/voice/my_commands/generators/list_generator'


class PriorityList < ListGenerator
  # @table maps canonicalize(spoken) to [[spoken, written, priority], ...]

  def initialize
    super()
    @table = Hash.new()
  end


  def add(spoken, written, priority=0)
    key        = canonicalize(spoken)
    candidates = ( @table[key] ||= Array.new() )

    # add if new written otherwise replace old one if new priority higher:
    old_one = candidates.select { |s,w,p| w==written }
    if old_one.length == 0 then
      @table[key].push([spoken, written, priority])
    elsif old_one[0][2] < priority then
      @table[key] = candidates.collect do |s,w,p|
        w==written ? [spoken, written, priority] : [s,w,p]
      end
    end
  end


  def generate_entries
    entries = []
    @table.each do |key, candidates|
      # stable sort by priority, greatest first:
      i = 0
      candidates = candidates.sort_by { |s,w,p| [-p,i += 1] }

      # keep all candidates, but all but first is disabled:
      entries << [candidates[0][0], candidates[0][1], false]
      candidates[1..-1].each { |s,w,p| entries << [s,w,true] }
    end

    entries
  end
end
