#SingleInstance Force

f9::
 SendMode Event
 Send AutoHotkey SendMode Event:{enter}
 Loop, 5 {
  Loop, 10 {
    Send .{space}
    Send {,}{space}
  }
  Send {!}{enter}
 }
 Send {space}{enter}
 return


f10::
 SendMode Input
 Send AutoHotkey SendMode Input:{enter}
 Loop, 5 {
  Loop, 10 {
    Send .{space}
    Send {,}{space}
  }
  Send {!}{enter}
 }
 Send {space}{enter}
 return


f11::
 SendMode Play
 Send AutoHotkey SendMode Play:{enter}
 Loop, 5 {
  Loop, 10 {
    Send .{space}
    Send {,}{space}
  }
  Send {!}{enter}
 }
 Send {space}{enter}
 return

f12::
 SendMode Play
 Send AutoHotkey SendMode Play:{enter}
 Loop, 4 {
  Loop, 25 {
    Send D{space}
  }
  Send {space}{enter}
 }
 Send {space}{enter}
 return

