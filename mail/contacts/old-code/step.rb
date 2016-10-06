##
## 
##

require './rolodex'


#
# read in rolodex:
#
R = Rolodex.load(File.open("addresses"))



R.dump(File.open("addresses.ROL", "w"))
