### 
### Quick extension to allow holding and releasing the control key:
### 

import win32api, win32con

# Vocola procedure: Keyboard.Hold_Control
def hold_control():
    win32api.keybd_event(win32con.VK_CONTROL, 0, 0, 0)

# Vocola procedure: Keyboard.Release_Control
def release_control():
    win32api.keybd_event(win32con.VK_CONTROL, 0, win32con.KEYEVENTF_KEYUP, 0)



import SendKeysCtypes

# Vocola procedure: Keyboard.SendKeys
def sendkeys(keys):
    SendKeysCtypes.SendKeys(keys, pause=0.00)
