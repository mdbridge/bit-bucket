;;; 
;;; Script to reposition wayward DNS 11+ correction-menu windows closer to
;;; the window of the application being corrected.
;;; 

#SingleInstance force


; is [X, X+W) entirely visible and not split across monitors?
Visible(X, W)
{
  If (X < 0) {
    return FALSE
  }    

  ; the below assumes two monitors, with the primary monitor to the left:

  If (X < A_ScreenWidth and X+W>A_ScreenWidth) {
    return FALSE
  }    

  If (X+W > A_ScreenWidth*2) {
    return FALSE
  }    

  return TRUE
}	


Move(Xo, Yo, Wc, Wo, Hc, Ho)     ; _o = app, _c = correction menu
{
  Y := Yo
  ; adjust Y so that [Y, Y+Hc) is as much on screen as possible:
  if (Y+Hc > A_ScreenHeight) {
    Y := A_ScreenHeight - Hc
  }    
  if (Y < 0) {
    Y := 0
  }    


  ; try immediately to left:
  If (Visible(Xo-Wc, Wc)) { 
    WinMove, Xo-Wc, Y
    return
  } 

  ; try right side of first monitor if app on second monitor:
  if (Xo > A_ScreenWidth and Visible(A_ScreenWidth-Wc, Wc)) {
    WinMove, A_ScreenWidth-Wc, Y
    return
  } 

  ; try immediately to right:
  if (Visible(Xo+Wo, Wc)) {
    WinMove, Xo+Wo, Y
    return
  } 

  ; try left side of second monitor if app on first monitor:
  if (Xo+Wo < A_ScreenWidth and Visible(A_ScreenWidth, Wc)) { 
    WinMove, A_ScreenWidth, Y
    return
  } 

  ; fallback: on top of app:
  WinMove, Xo, Y
}	


Loop
{
  WinWait, Choice Box
  WinGetPos, Xc, Yc, Wc, Hc      ; for choice box

  ; is correction menu wayward?  I.e., at (0, 0-1)?
  wayward := true
  If Xc
  {
      wayward := false
  }
  If Yc>1
  {
      wayward := false
  }

  
  If wayward
  {
    WinGetPos, Xo, Yo, Wo, Ho, A ; for original window
    Move(Xo, Yo, Wc, Wo, Hc, Ho)
  }	

  Loop, 10
  {
    Sleep, 100
    IfWinExist, Choice Box
    {
      continue
    }
  }
}
