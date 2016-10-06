###
### Voice commands for controlling Dragon 14 vocabulary editor
###

import time, sys
import win32gui

import natlink
from VocolaUtils import VocolaRuntimeError

import vocola_ext_clipboard



# Vocola procedure: Dragon.open_properties
def vocola_open_properties(written, spoken):
    #print " opening properties for %s\%s" % (written, spoken)
    if spoken == "":
        spoken = written

    wait_for_title("Vocabulary Editor")
    time.sleep(0.05)
    play("{alt+r}")
    play(spoken)
    play("{tab}")
    if len(written) == 1:
        play(written)
    else:
        play("{up}")

    tries = 10
    while tries > 0:
        play("{alt+p}")
        wait_for_title("Word Properties")
        if is_word(written, spoken):
            return

        play("{esc}")
        wait_for_title("Vocabulary Editor")
        play("{down}")
        tries -= 1
    raise VocolaRuntimeError("Unable to find word %s\%s" % (written, spoken))

def is_word(written, spoken):
    found_written = get_clipboard("{alt+o}{shift+tab}{ctrl+c}")
    found_spoken  = get_clipboard("{shift+tab}{ctrl+c}")
    if written==found_written and spoken==found_spoken:
        print "  found %s\%s" % (found_written, found_spoken)
    else:
        print >> sys.stderr, "  found %s\%s" % (found_written, found_spoken)
    return written==found_written and spoken==found_spoken



def wait_for_title(title, timeout=15):
    try:
        timeout = int(timeout)
    except ValueError:
        raise ValueError("unable to convert '" + timeout.replace("'", "''") +
                         "' into an integer")
    delay = 0.05
    while timeout > 0:
        if window_title() == title:
            return
        time.sleep(delay)
        timeout -= delay
    raise Timeout("A timeout occurred while waiting for the window '%s'" % (title))

def window_title():
    handle = win32gui.GetForegroundWindow()
    return win32gui.GetWindowText(handle)

class Timeout(Exception):
    pass



def play(keys):
    natlink.playString("{shift}" + keys)



def get_clipboard(keys):
    vocola_ext_clipboard.clipboard_set("34262245232")
    play(keys)
    vocola_ext_clipboard.clipboard_wait_for_new("34262245232")
    return vocola_ext_clipboard.clipboard_get()
