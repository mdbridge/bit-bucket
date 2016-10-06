import sys
import thread
import time
import traceback
import wx

from natlink import *


app = wx.App(False)

def close(frame = None, event = None):
    natDisconnect()
    print 'natlink disconnected'
    sys.exit()
    app.ExitMainLoop()


class RedirectText:
    def __init__(self, aWxTextCtrl, standard_error = False):
        self.out            = aWxTextCtrl
        self.standard_error = standard_error
    
    def write(self, string):
        start = self.out.GetLastPosition()
        self.out.AppendText(string)
        end   = self.out.GetLastPosition()
        if self.standard_error:
            self.out.SetStyle(start, end, wx.TextAttr("RED", "WHITE")) 


last_module = []
def heartbeat():
    global last_module
    #print 'heartbeat!'
    try:
        current_module = getCurrentModule()
        if current_module != last_module:
            last_module = current_module
            print "virtually toggling microphone..."
            natlinkmain.changeCallback("mic", "on")
            call_got_begin(current_module)
    except:
        traceback.print_exc()

def call_got_begin(module_info):
    for module in sys.modules.keys():
        try:
            sys.modules[module].thisGrammar.beginCallback(module_info)
            #print " called gotBegin for " + module
        except:
            pass


class Application(wx.Frame):
    def __init__(self, parent):
        wx.Frame.__init__(self, parent, -1, 
                          'NatLink messages window - close to exit NatLink', 
                          size=(550, 450))
        panel = wx.Panel(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        panel.SetSizer(sizer)
        
        self.log = wx.TextCtrl(panel, -1, size=(500,400),
                               style = wx.TE_MULTILINE | wx.TE_READONLY 
                                     | wx.HSCROLL | wx.TE_RICH2)
        
        sizer.Add(self.log, 0, wx.TOP|wx.LEFT|wx.EXPAND)        
        sys.stdout = RedirectText(self.log)
        sys.stderr = RedirectText(self.log, True)
        
        # Using monospace font:
        style = self.log.GetDefaultStyle()
        font = wx.Font(8, wx.MODERN, wx.NORMAL, wx.NORMAL)
        style.SetFont(font)
        self.log.SetDefaultStyle(style)
        print 'Starting up...'
        print
        
        self.timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.update, self.timer)
        self.Bind(wx.EVT_CLOSE, close)        
        self.Centre()
        self.Show(True)
        self.timer.Start(250)        

    def update(self, event):
        heartbeat()


Application(None)

natConnect(1)
import natlinkmain   # sets up NatLink as a side effect of being imported

try:
    app.MainLoop()    
except:
    #traceback.print_exc()
    pass
finally:
    close()
