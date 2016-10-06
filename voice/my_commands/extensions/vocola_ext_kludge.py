### 
### Module Kludge:
### 

# for debugging, import fresh copy of SendInput each time we load extensions:
import sys
try:
    del sys.modules["SendInput"]
except:
    pass
#print "importing SendInput..."

import SendInput
from SendInput import *

# for debugging, import fresh copy of ExtendedSendDragonKeys each time
# we load extensions:
try:
    del sys.modules["ExtendedSendDragonKeys"]
except:
    pass
#print "importing ExtendedSendDragonKeys..."

import ExtendedSendDragonKeys



### 
### 
### 

# Vocola procedure: Kludge.SendInput
def send_input(specification):
    SendInput.send_input(
        ExtendedSendDragonKeys.senddragonkeys_to_events(specification))


# Vocola procedure: Kludge.Send,1-
def do(*keys):
    specification = ""
    for key in keys:
        if key[0]!= "{" or len(key) == 1:
             key = "{" + key + "}"
        specification += key
    send_input(specification)


#   Experimental:
# Vocola procedure: Kludge.Unicode
def p(key):
    code = long(key, 16)
    SendInput.send_input([Unicode_event(code, False), Unicode_event(code, True)])

# Vocola procedure: Kludge.Wheel
def wheel(amount):
    amount = int(amount)
    SendInput.send_input([mouse_wheel_event(False, amount)])
