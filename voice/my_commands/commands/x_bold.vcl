###
### Voice commands for the bold project
###

show stack trace = "gdb -silent test core"{enter} "bt 10"{enter} quit{enter};
show thread test stack trace = "gdb -silent thread-test core"{enter} "bt 10"{enter} quit{enter};

make [(thread=thread-)] (single|parallel|crashing) (random|advance) here =
    "make $1$2-$3"{enter};


AUTO2(arguments, command) := 
    "autobuild.rb $arguments 'unbuffer $command 2>&1 | head -n 60'{enter}";
AUTO(arguments) := AUTO2($arguments, make);

autobuild here 	    = AUTO("");
autobuild bold here = AUTO("-t ~/the_machine/bold");
