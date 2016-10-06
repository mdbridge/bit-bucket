import wx
import sys
import thread
import time
import traceback


from natlink import *   

natConnect(1)

import natlinkmain  # sets up NatLink as a side effect of being imported


last_module = []
def heartbeat():
    global last_module
    print 'heartbeat!'
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
            print "  called gotBegin for " + module
        except:
            pass


class Application(wx.Frame):
    def __init__(self, parent):
        wx.Frame.__init__(self, parent, -1, 'NatLink heartbeat control', size=(500, 100))
        panel = wx.Panel(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        panel.SetSizer(sizer)
        txt = wx.StaticText(panel, -1, 'The button above the toggles the heartbeat timer on and off.')
        sizer.Add(txt, 0, wx.TOP|wx.LEFT, 20)

        self.timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.update, self.timer)
        self.toggleBtn = wx.Button(panel, wx.ID_ANY, "Start")
        self.toggleBtn.Bind(wx.EVT_BUTTON, self.onToggle)

        self.Centre()
        self.Show(True)

    def onToggle(self, event):
        btnLabel = self.toggleBtn.GetLabel()
        if btnLabel == "Start":
            print "starting timer..."
            self.timer.Start(250)
            self.toggleBtn.SetLabel("Stop")
        else:
            print "timer stopped!"
            self.timer.Stop()
            self.toggleBtn.SetLabel("Start")

    def update(self, event):
#        print "\nupdated: ",
#        print time.ctime()
        heartbeat()

app = wx.App(0)
Application(None)
app.MainLoop()

natDisconnect()
print 'natlink disconnected'
import sys
sys.exit()
