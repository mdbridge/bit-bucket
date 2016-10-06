# patch to allow sorted keys in yaml hashes

class Hash
  def to_yaml( opts = {} )
    YAML::quick_emit( object_id, opts ) do |out|
      out.map( taguri, to_yaml_style ) do |map|
        sorted_keys = keys
        sorted_keys = begin
          sorted_keys.sort
        rescue
                        sorted_keys.sort_by {|k| k.to_s} rescue sorted_keys
        end

        sorted_keys.each do |k|
          map.add( k, fetch(k) )
        end
      end
    end
  end
end
