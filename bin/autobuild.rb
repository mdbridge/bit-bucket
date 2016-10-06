#!/usr/bin/env ruby

##
## autobuild
##
##
##
##
## Partially inspired by Micah Elliott's automake.py.
##

require "rb-inotify"


##
## Building
##

$building = false
$changes  = "initial startup"

RED_ASCII     = "\e[30;41;5m"  # blink in xterm
GREEN_ASCII   = "\e[30;42;2m"
YELLOW_ASCII  = "\e[30;43;2m"
NOCOLOR_ASCII = "\e[0m"

def format text, symbol, color
    while text.length < 80-(symbol.length*2)
      text = symbol +  text + symbol
    end
  color + text + NOCOLOR_ASCII
end

def build build_command
  system("clear")
  puts format(" autobuild: #{Time.now} ", "?", YELLOW_ASCII)
  puts "  WHY: #{$changes}"

  $building = true
  $changes  = nil

  puts build_command
  result = system(build_command)
  if result
    print format(" GREEN BAR ", "+", GREEN_ASCII)
  else
    print format(" RED BAR ",   "+", RED_ASCII)
  end

  drain_events
  $building = false
end



##
## Watching for changes
##

def reset_watch_history
  $sources = {}
  $outputs = {}
end


def setup_watching targets
  reset_watch_history

  $notifier = INotify::Notifier.new
  targets.each do |target|
    $notifier.watch(target, :close_write,:create,:delete,:move,:recursive,
                            :delete_self,:move_self) do |event|
      if event.name =~ /~$/ or event.name =~ /^\#/ or event.name =~ /^\.\#/
        # pass
      elsif event.absolute_name =~ %r{/.(svn|git)*(/|$)}
        # pass
      elsif $building and not $sources[event.absolute_name]
        $outputs[event.absolute_name] = true
      else
        $sources[event.absolute_name] = true
        $changes = event.absolute_name
      end
    end
  end
end

def drain_events
  while IO.select([$notifier.to_io], [], [], 0.1)
    $notifier.process
  end
end


##
## Main routine
##

watch_targets = ["."]
while ARGV.size >= 2 and ARGV[0] == "-t"
  ARGV.shift
  watch_targets.push ARGV.shift
end

if ARGV.size > 0
  build_command = ARGV.join(" ")
else
  build_command = "make"
end


setup_watching(watch_targets)
loop do
  begin
    build(build_command) if $changes
    drain_events
  rescue Interrupt
    puts "Hit ^C again to really quit; restarting shortly..."
    $changes = "^C hit"
    reset_watch_history
    begin
      sleep 2
    rescue Interrupt
      $notifier.stop
      break
    end
  end
end
