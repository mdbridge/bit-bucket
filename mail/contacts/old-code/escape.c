main(argc,argv)
char **argv;
int argc;
{
    int in_quote = 0;

    int c;
    while ((c = getchar()) != -1) {
	if (c == '"')
	    in_quote = !in_quote;

	if (in_quote && (c == '\n')) {
	    putchar('\\');
	    putchar('n');
	    continue;
	}

	if (in_quote && (c == 13)) {
	    putchar('\\');
	    putchar('r');
	    continue;
	}

	if (in_quote && (c == '\t')) {
	    putchar('\\');
	    putchar('t');
	    continue;
	}

	/* Remove carriage returns in case of DOS files */
	if (c == 13)
	  continue;

	putchar(c);
    }
}
