###
### Extension for timing Vocola command (parts)
###

import time


# Vocola procedure: Benchmark.Start
def set_time():
    global t, c
    t = time.time()
    c = time.clock()

# Vocola function: Benchmark.Stop
def measure_time():
    global t, c
    cpu = time.clock() - c
    wall = time.time() - t
    return "%4.2f CPU  %4.2f wall" % (cpu, wall)
