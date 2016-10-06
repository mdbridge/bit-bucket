### 
### Utility program for number_files:
### 
###   Displays input lines in columns with numbers, handling color
###   escape codes correctly.
### 

# Attempt to compute actual width of window:
window_width = 80
try          = `bash -c 'echo $COLUMNS'`   # gives "" on cygwin
window_width = try.to_i if try =~ /^\d+$/


def strip_color(text)
  text.gsub(/\e\[[^m]*m/, "")
end


def label_length(i)
  i.to_s().length
end

def make_label(i, field_width)
#  (" "*field_width + i.to_s())[-field_width..-1]
  # color labels in gray:
  number = i.to_s()
  " "*(field_width - number.length) + "\e[1;30m" + number + "\e[0m"
end


Column_separator = "  "
Label_postfix    = " "

def display(items, lengths, stride, widths=nil)
  dummy, widths = compute_widths(lengths, stride) unless widths!=nil

  label_widths = 1.upto(widths.length).collect { |i| label_length(stride*i+1-1) }
  label_widths[-1] = label_length(items.length)  # last column can be short

  0.upto(stride-1) do |offset|
    result = ""
    0.upto(widths.length-1) do |j|
      i = offset + stride*j
      next if i>=items.length
      item = items[i]

      result += Column_separator unless j == 0

      result += make_label(i+1, label_widths[j]) + Label_postfix +
                item + " "*(widths[j] - lengths[i])
    end
    puts result
  end
end

def display_single_overwide_column(items)
  label_width = label_length(items.length)

  items.each_with_index do |item, i|
    puts make_label(i+1, label_width) + Label_postfix + item
  end
end

def compute_widths(lengths, stride)
  widths = []
  maximum_display_width = 0

  offset = 0
  while offset < lengths.length do
    max_item  = lengths[offset ... offset + stride].max
    label = label_length(offset + stride-1)  #

    widths.push(max_item)
    maximum_display_width += Column_separator.length unless offset == 0
    maximum_display_width += label + Label_postfix.length + max_item

    offset += stride
  end

  return maximum_display_width, widths
end



colored_items = []
item_lengths  = []

$stdin.each do |colored_line|
  colored_line.chomp!
  uncolored = strip_color(colored_line)
  next if uncolored == ""

  colored_items.push(colored_line)
  item_lengths.push(uncolored.length)
end


stride = item_lengths.length
1.upto(item_lengths.length) do |s|
  maximum, widths = compute_widths(item_lengths, s)
  if maximum < window_width then
    display(colored_items, item_lengths, s)
    exit(0)
  end
end
display_single_overwide_column(colored_items)
