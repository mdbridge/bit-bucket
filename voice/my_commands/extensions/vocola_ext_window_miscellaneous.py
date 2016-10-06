### 
### Module Window part III: changing window state, size, and position
### 

import win32gui, win32con
import ctypes

from vocola_ext_window_status import set_status, status



##
## Retrieving the current state of the active window:
##

# Vocola function: Window.Minimized
def minimized():
    handle = win32gui.GetForegroundWindow()
    if win32gui.IsIconic(handle):
        return "true"
    else:
        return "false"

# Vocola function: Window.Maximized
def maximized():
    handle = win32gui.GetForegroundWindow()
    # win32gui.IsZoomed does not exist so fall back on ctypes:
    if ctypes.windll.user32.IsZoomed(handle):
        return "true"
    else:
        return "false"


##
## Change the state of the active window.
##
## These don't work for elevated applications, failing silently other
## than setting Window.Success() in that case.
##

# Vocola procedure: Window.Minimize
def minimize():
    set_status(False)
    handle = win32gui.GetForegroundWindow()
    win32gui.ShowWindow(handle, win32con.SW_MINIMIZE)
    set_status(win32gui.IsIconic(handle))

# Vocola procedure: Window.Maximize
def maximize():
    set_status(False)
    handle = win32gui.GetForegroundWindow()
    win32gui.ShowWindow(handle, win32con.SW_MAXIMIZE)
    set_status(ctypes.windll.user32.IsZoomed(handle))

# Vocola procedure: Window.Restore
def restore():
    set_status(False)
    handle = win32gui.GetForegroundWindow()
    # ensure window is visible for non-elevated applications:
    win32gui.ShowWindow(handle, win32con.SW_SHOW)   
    result = win32gui.ShowWindow(handle, win32con.SW_RESTORE)
    # If says previously invisible, then we are dealing with an
    # elevated application and have failed:
    set_status(result != 0)  

# Vocola procedure: Window.Close
def close():
    set_status(False)
    handle = win32gui.GetForegroundWindow()
    try:
        win32gui.PostMessage(handle,win32con.WM_CLOSE,0,0)
        set_status(True)
    except Exception:
        pass



##
## Getting the size and position of the active window:
##

# Vocola function: Window.GetPosition
def get_position():
    handle = win32gui.GetForegroundWindow()
    left, top, right, bottom = win32gui.GetWindowRect(handle)
    return "%d,%d" % (left, top)

# Vocola function: Window.GetSize
def get_size():
    handle = win32gui.GetForegroundWindow()
    left, top, right, bottom = win32gui.GetWindowRect(handle)
    width  = right - left
    height = bottom - top
    return "%d,%d" % (width, height)


##
## 
##

# can't move elevated Windows
# Vocola procedure: Window.Move
def move_window(position):
    set_status(False)
    x,y = position.split(",")
    x = int(x)
    y = int(y)
    handle = win32gui.GetForegroundWindow()
    left, top, right, bottom = win32gui.GetWindowRect(handle)
    width  = right - left
    height = bottom - top
    win32gui.MoveWindow(handle, x, y, width, height, 1)
    set_status(True)

# can't resize elevated Windows
# Vocola procedure: Window.Resize
def resize_window(size):
    set_status(False)
    width,height = size.split(",")
    width  = int(width)
    height = int(height)
    handle = win32gui.GetForegroundWindow()
    left, top, right, bottom = win32gui.GetWindowRect(handle)
    win32gui.MoveWindow(handle, left, top, width, height, 1)
    set_status(True)
