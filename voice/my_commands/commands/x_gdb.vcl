###
### Voice commands for controlling gdb
###

### 
### Experiment: debugging express tests via gdb
### 

include "gnu.vch";


## 
## Starting up gdb:
## 

# gdb program [core]
# gdb program <process ID>     # attempt to attach to a running process
# gdb --args <command>
# [inside Emacs] Do(gdb) EraseToStart() "gdb -i=mi --args <path_to_executable> <executable_arg>*"

# run [args]: auto reloads if executable changed; restarts; args can
#             include shell wildcards, redirection but not pipelines
#             defaults to last arguments
# set args <args>: set default for run

# -silent removes annoying text at startup



## 
## Examining the stack:
## 

  # list current stack trace of current thread
GDB stack trace [[(minus=-)] 1..50] = "bt " When($2,$1$2,10) {enter};  # also where

  # change current stack frame (changes variables in scope)
GDB (up|down|frame) [0..20] = "$1 $2"{enter};



## 
## Other inspection/modification:
## 

# list: show source lines around current point

# info args, locals, threads

# thread <thread number>

# print <expr>; including things like x=f(y); fields are this.x
# set variable x = 100

# info proc mappings



## 
## Breakpoints:
## 

# break [filename:]func_name
# break btree_page::compact
# break [btree_page.cpp:]14   # defaults to current source file
# break *expression
# break                 # put breakpoint at next instruction in current stack frame
# rbreak [filename:]<regexp>

# break ... if <cond>
# condition <bnum> [expression]

# dprintf location, template, expr...
#  ex: dprintf 25,"at line 25, glob=%d\n",glob


  # use -l to continue watching even when variables go out of scope:
# watch <expr>     # break when <expr> changes value
# rwatch <expr>    # break when <expr> is read

# catchpoints also exist for things like throwing exceptions, calling
# system calls, signals, and the like; help catch
# breakpoints can also be thread specific


# info breakpoints, watchpoints

# disable/enable <breakpoints> [once]

# clear [location]
# delete <breakpoint_number>

# can execute commands (including display and continue) when point
# triggers; help commands


  # n[ext]: run until next line in current function
  # s[tep]: into subroutine ...
  # these and most commands can be repeated by pressing return
GDB (next [line]=n|step [into]=s|into=s|continue=c) [1..10] = 
    Repeat(When($2,$2,1), $1{enter} Wait(500));

# finish  # stop just after current function returns

  # until skips recursive function calls; advance doesn't
# until/advance <location> # stops early at the end of current function

# can automatically skip certain functions/files: help skip

  # prevent other threads from running while stepping one thread:
# set scheduler-locking step 

# display <expr>: auto print at each breakpoint



## 
## Miscellaneous:
## 

# help [<category>]
# quit



# make make-args

# its gnu readline...   including completion of symbols	via tab


##
## Debugging libc:
##

#  # this puts glibc-2.19 in the current directory on Debian:
#apt-get source libc6
#
#  # in gdb itself:
#directory glibc-2.19/sysdeps/unix
#directory glibc-2.19/csu
#directory glibc-2.19/stdlib
#directory glibc-2.19/elf
