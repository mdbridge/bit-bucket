### 
### Module mouse:
### 

import win32api
import win32gui
import natlink;
import VocolaUtils


def internal_move(x, y):
    # The below doesn't work when an elevated application has focus:
    #win32api.SetCursorPos((x,y))
    VocolaUtils.call_Dragon("SetMousePosition", "iii", [0, x, y])

def click(button):
    VocolaUtils.call_Dragon("ButtonClick", "ii", [button, 1])

def position():
    return natlink.getCursorPos()


def anchor(kind=None):
    left, top = 0, 0
    if kind:
        if kind == "absolute":
            left, top = 0, 0
        elif kind == "window":
            name, title, hwnd = natlink.getCurrentModule()
            left, top, right, bottom = win32gui.GetWindowRect(hwnd)
        elif kind == "interior":
            name, title, hwnd = natlink.getCurrentModule()
            left, top = win32gui.ClientToScreen(hwnd, (0, 0))
        elif kind == "relative":
            left, top = position()
    return left, top


def convert_coordinate(kind, x, y):
    left, top = anchor(kind)
    x += left 
    y += top
    print "%s: %s -> %s" % ((kind,x,y), position(), (x,y))
    return x, y



def parse(args):
    new_args = []
    for arg in args:
        new_args += arg.split(",")
    args = new_args
    print repr(args)

    button, kind, x, y = None, None, None, None
    if len(args) > 0:
        if args[0] == "right":
            button = 2
            args = args[1:]
    if len(args) > 2:
        kind = args[0]
        args = args[1:]
    if len(args) >= 2:
        x = int(args[0])
        y = int(args[1])
        x, y = convert_coordinate(kind, x, y)
    return button, kind, x, y



# Vocola procedure: Mouse.Go,1-3
def mouse_go(*args):
    button, kind, x, y = parse(args)
    internal_move(x,y)

# Vocola procedure: Mouse.Click,0-4
def mouse_click(*args):
    button, kind, x, y = parse(args)
    if x: internal_move(x,y)
    if not button: button = 1
    click(button)

# Vocola function: Mouse.Position,0-1
def mouse_position(kind=None):
    left, top = anchor(kind)
    x, y = natlink.getCursorPos()
    if not kind or kind == "absolute":
        return "%d,%d" % (x-left, y-top)
    else:
        return "%s,%d,%d" % (kind, x-left, y-top)









# Vocola function: Mouse.Get
def mouse_get():
    position = natlink.getCursorPos()
    return "%d,%d" % position

# Vocola function: Mouse.GetX
def mouse_get_x():
    return natlink.getCursorPos()[0]

# Vocola function: Mouse.GetY
def mouse_get_y():
    return natlink.getCursorPos()[1]


# Vocola procedure: Mouse.Set
def mouse_set(position):
    x,y = position.split(",")
    VocolaUtils.call_Dragon("SetMousePosition", "iii", [0, x, y])




## To do: Pair.first, second, make?
##        use natlink.getScreenSize()


import win32api, win32con

# WARNING: primary/secondary button swap occurs after this...

# Vocola procedure: Mouse.Hold_left
def hold_left_button():
    import time
    win32api.mouse_event(win32con.MOUSEEVENTF_LEFTDOWN, 0, 0)
    time.sleep(0.05)
    win32api.mouse_event(win32con.MOUSEEVENTF_LEFTUP, 0, 0)

# Vocola procedure: Mouse.Hold_right
def hold_right_button():
    win32api.mouse_event(win32con.MOUSEEVENTF_RIGHTDOWN, 0, 0)

# Vocola procedure: Mouse.Release_right
def release_right_button():
    win32api.mouse_event(win32con.MOUSEEVENTF_RIGHTUP, 0, 0)

# see URL: mouse_event function in Vocola for more information
# see http://msdn.microsoft.com/en-us/library/windows/desktop/ms646260(v=vs.85).aspx for information to create other events



import win32gui

# Vocola function: Mouse.GetLocal
def mouse_get_local():
    x, y = natlink.getCursorPos()
    name, title, hwnd        = natlink.getCurrentModule()
    left, top, right, bottom = win32gui.GetWindowRect(hwnd)
    return "%d,%d" % (x-left, y-top)

# Vocola procedure: Mouse.SetLocal
def mouse_set_local(position):
    x,y = position.split(",")
    name,title,hwnd = natlink.getCurrentModule()
#    left,top,right,bottom = win32gui.GetWindowRect(hwnd)
#    x += left
#    y += top
    VocolaUtils.call_Dragon("SetMousePosition", "iii", [1, x, y])


# Vocola procedure: Mouse.SetScaled
def mouse_set_scaled(position):
    x,y = position.split(",")
    name, title, hwnd        = natlink.getCurrentModule()
    left, top, right, bottom = win32gui.GetWindowRect(hwnd)
    x = int(int(x)*1.0/100*(right-left-1))
    y = int(int(y)*1.0/100*(bottom-top-1))
    VocolaUtils.call_Dragon("SetMousePosition", "iii", [1, x, y])
