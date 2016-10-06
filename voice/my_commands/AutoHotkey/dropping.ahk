#SingleInstance Force

f9::
 SendMode Event
 Loop, 8 {
  Loop, 7 {
    Send 0
    Send 1
    Send 2
    Send 3
    Send 4
    Send 5
    Send 6
    Send 7
    Send 8
    Send 9
  }
  Send {enter}
 }
 return


f10::
 SendMode Input
 Loop, 8 {
  Loop, 7 {
    Send 0
    Send 1
    Send 2
    Send 3
    Send 4
    Send 5
    Send 6
    Send 7
    Send 8
    Send 9
  }
  Send {enter}
 }
 return


f11::
 SendMode Play
 Loop, 8 {
  Loop, 7 {
    SendPlay 0
    Send 1
    Send 2
    Send 3
    Send 4
    Send 5
    Send 6
    Send 7
    Send 8
    Send 9
  }
  Send {enter}
 }
 return

