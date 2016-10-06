from vocola_ext_variables import *


### 
### Experimental:
### 


import sys

# Vocola procedure: Experimental.Reset
def reset_extensions():
    del sys.modules["vocola_ext_test"]
    del sys.modules["vocola_ext_mouse"]


import os

# Vocola function: Ext.get_result
def get_result():
    result = os.getenv("HOME") + r'\AutoHotkey\result.txt'
    FILE = open(result, "r")
    line = FILE.readline()
    FILE.close()
    return line


import _vocola_main

# Vocola procedure: Vocola.Load
def load( file ):
    _vocola_main.thisGrammer.loadFile( file )


import hashlib

# Vocola function: Experimental.Hash
def calculate_MD5(text):
    return hashlib.md5(text).hexdigest()
