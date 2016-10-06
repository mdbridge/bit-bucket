### 
### Module Window part I: status reporting
### 

success = True                  # success status of last Window procedure

def set_status(status):
    global success
    success = status

def status():
    global success
    return success


# Vocola function: Window.Success
def window_success():
    global success
    if success:
        return "true"
    else:
        return "false"
