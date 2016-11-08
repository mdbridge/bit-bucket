### 
### Voice commands for Windows 10 task manager
### 

# can invoke via {shift+ctrl+esc}

# click process/performance/...

show (process|processes) [(name=0|CPU=1|memory=2|disk=3|network=4)] = 
    HeardWord(click, Processes) Wait(300)
       # click on "name" column heading:
    Mouse.Click(interior, 26, 60)
    {right_ When($2,$2,0) } {enter};
