# Last updated 7/13/2016

# The commands in this file get executed whenever a new shell starts.  This
# is the place to do set, setenv and alias commands.  
#


######################################################################
#                                                                    #
# Switch to tsch if appropriate:                                     #
#                                                                    #
######################################################################

#
# assume interactive shell until we prove otherwise
#
set interactiveshell
if ($?0 || ! $?prompt) then
	#
	# noninteractive shells (shell scripts) have argv[0] values,
	# or they don't have prompts (or both, but who cares?)
	#
	unset interactiveshell
else
	#
	# tty returns success only if stdin is a terminal of some kind
	#
	tty -s || unset interactiveshell
endif

unset hwterm
set tty=`tty`
if ($?interactiveshell) then
	if ($tty =~ /dev/tty[0-9][0-9]) set hwterm
endif

if (! $?EMACS && ! $?hwterm && $?interactiveshell && ! $?tcsh) then
	#
	# if we're running in a GNU EMACS shell window, we don't want to
	# run tcsh since it can't deal with that kind of PTY for some reason.
	# and it doesn't work on hardwired lines.
	# likewise we don't want tcsh on shell scripts (why bother?), we
	# don't want tcsh if we're already in tcsh, and we don't want it
	# if it doesn't exist (repressive but safe).
	#
	# if we want it, exec it.  we will start back at the top of this
	# .cshrc file except that $tcsh will be set so when we get back
	# here, we'll skip this part.
	#

	if (-x /usr/local/bin/tcsh) then
	    exec /usr/local/bin/tcsh
	else if (-x /usr/bin/tcsh) then
	    exec /usr/bin/tcsh
	else if (-x /bin/tcsh) then
	    exec /bin/tcsh
	else
	    echo "No tcsh found\!"
	endif
endif


######################################################################
#                                                                    #
# Environment variables:                                             #
#                                                                    #
######################################################################


if (! $?PATH_SET) then
    setenv PATH_SET true

    set hostname = `/bin/hostname`
    set local = $HOME/bin
    if ($hostname == ts-rhel6) then
	setenv	PRINTER	pal01p137  # Left
	set local = $HOME/bin_6
    else if ($hostname =~ ts-rhel7*) then
	setenv	PRINTER	pal01p137  # Left
	set local = $HOME/bin_7
    else if ($hostname =~ lark*) then
	set local = $HOME/bin_lark
    else if ($hostname =~ build-debian*) then
	set local = $HOME/bin_Debian
    else if ($hostname =~ build-fedora*) then
	set local = $HOME/bin_fedora
    else if ($hostname =~ build-l4tm*) then
	set local = $HOME/bin_L4TM
    else if ($hostname =~ foil) then
	setenv	PRINTER	ENVY_4500
    else
	# pass
    endif
    set path = ($local $local/ruby-bin $HOME/bin . $path /usr/local/sbin /usr/sbin /sbin)
endif


if (! $?DISPLAY && $?term) then
    if ($term =~ *xterm) then
        # We're an xterm with no DISPLAY.  Default to screen 0 on the remote
        # host.  This is not completely correct, but what else can we do?
        if ($?REMOTEHOST) then
	    setenv DISPLAY ${REMOTEHOST}:0.0
        endif
    endif
endif


setenv	LESS   "-c -s -m -e -X"

setenv	MORE	"-c -s"			# -e not supported on Linux

#setenv MAIL /usr/spool/mail/$USER
setenv	MAIL	$HOME/Mail/incoming

if (! $?EDITOR) then
    setenv	EDITOR  emacs
endif

# default to turn on GCC coloring when output is to terminal:
setenv GCC_COLORS 'error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'


######################################################################
#                                                                    #
# Terminal settings:                                                 #
#                                                                    #
######################################################################

# These may need modification on hardwired terminals

if ($?interactiveshell) then
    # need SHELL set right for tset to work: 
    setenv SHELL /bin/csh
    set noglob
    eval `tset -s -Q`
    unset noglob

    stty erase "^h" kill "^u" intr "^c" eof "^D" susp "^z" echoe 
    stty ixon ixoff tostop

    #tabs	# need for spica?
endif


######################################################################
#                                                                    #
# [T]Csh customizations:                                             #
#                                                                    #
######################################################################

# Prevent dumping core by default:
limit	coredumpsize	0

# Configure history:
set history = 100               # how many commands to remember
#set savehist = 100
#unset	edithist		# Turn off annoying history edit
set histdup = erase             # use LRU on repeated commands
set autoexpand                  # allow tab to expand history

# Tell shells to notify you immediately of changes in background jobs:
set notify

# Prevent ^D from logging you out:
set ignoreeof

# Prevent auto logout:
unset autologout

# Make yourself the only one who can write on new files you create:
umask 022

set host=`/bin/hostname | /bin/sed 's/\..*$//'`

# using newline in prompt produces extra spaces when scraping xterms
alias precmd 'echo'
if (-w /.) then
    set	prompt="%B$host [\!]#%b "
    else
    set	prompt="%B$host [\!]%b% "
endif

set globstar             # allow use of "**"

set echo_style = both    # allow -n AND \e with echo


######################################################################
#                                                                    #
# Aliases:                                                           #
#                                                                    #
######################################################################

# Define an alias for resetting the size of a window
#    (the location of resize is system dependent)
#alias resize 'set noglob; eval `/usr/bin/X11/resize \!*`; unset noglob'

#
# Ensure .newsrc is rw-------, use mailbox format for saves
#
alias	rn	'umask 77; \rn -M'

#
# Overriding of commands/change default options:
#
    # only need sed below for SRC, which won't take 2 u options...
alias	ps		'/bin/ps xu`echo \!* | sed "s/u//"`'

alias	cvs	'umask 022; /usr/bin/cvs \!*'
alias	df	'df -h'
alias	du	'du -h'  # was -k
alias   grep    'grep --color'
alias   ls      'ls --color=auto'
alias   ruby    'ruby -w'
alias   unison  'unison-2.48 -ui text'
#alias	where	which			# Until I get a real shell...

#
# Useful aliases:
#
alias	M		more
#alias	more		less
alias	,		suspend
alias	.		history
alias	j		jobs -l

#
# New commands:
#
alias	CD	'cd \!* && source ~/bin/number_files'
alias	cleanup	'(rm -- *~ .*~ #* *# *.bak; cd \!*; rm -- *~ .*~ #* *# *.bak)'
alias	cleanup_tree 'find . \( -name \*~ -o -name \#\* -o -name \*\# -o -name \*.bak \) -exec rm "{}" \;'
alias	drop	'touch \!*; mv \!* Out*/'
alias	down	'cd Out* && ls -st'
alias	athena		xtelnet charon.mit.edu
alias	cmu		xtelnet wily.fox.cs.cmu.edu

#
# Implement the ability to go back 1 directory:
#
set	previous="$cwd"
set	current="$cwd"
alias	cwdcmd		'set previous="$current"; set current="$cwd"'
alias	back		'set to="$previous"; cd "$to"; unset to; dirs'
