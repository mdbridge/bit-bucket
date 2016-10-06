### 
### Voice commands for Calibre e-books manager
### 

view specific format = {alt+v};

add books     = a;
edit metadata = e;



Libraries() := Mouse.Click(window, 765, 66);

quick switch = Libraries() {down_2} {right};


Edit Metadata:
  download metadata = {alt+d}{enter};  # doesn't work if in a field?
    # okay button appears to not be keyboard accessible:
  okay = Mouse.Click(window, 694, 679);
  next = Mouse.Click(window, 846, 674);
