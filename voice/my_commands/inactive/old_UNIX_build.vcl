## 
## CVS commands:
## 

  # use cvs to update the current directory:
#cvs update = "cvs update -d";



## 
## Controlling init.d daemons:
## 

#<target> := ( my SQL = "mysql"
#            | metabox
# 	     | poe
# 	     );
#
#<cmd> := (start | restart | stop | status);
#
#<cmd> <target> daemon = "sudo /etc/init.d/$2 $1{enter}";
