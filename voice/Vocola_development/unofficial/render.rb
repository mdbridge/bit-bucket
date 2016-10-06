### 
### Like haml <file>, but with simple Rails-style layout
### 

$VERBOSE = false  # hide spurious warnings from redcarpet

require 'redcarpet'
require 'haml'

#Haml::Options.defaults[:format] = :html4


fail unless ARGV.length == 1
name = ARGV[0]


def find_layout filename
  "_layout.html.haml"
end


class Collector
  def initialize
    @pieces = {}
  end

  def content_for piece, &block
    @pieces[piece] = capture_haml(&block)
  end

  def get_piece(piece)
    @pieces[piece]
  end
end


collector = Collector.new
view      = Haml::Engine.new(File.read(name)).render(collector)


layout_engine =  Haml::Engine.new(File.read(find_layout(name)))
combined = layout_engine.render(Object.new, view: name) do |piece|
  collector.get_piece(piece) || view
end
 
puts combined
