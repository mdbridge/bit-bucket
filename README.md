This repository contains a snapshot of Mark Lillibridge's voice control
infrastructure circa September 2016.


Its contents are currently completely uncurated; I hope later to clean
up pieces and publish them in other open source repositories.  The
contents include:

* New Vocola 2 extensions (interfacing to AutoHotkey, switching between
windows, changing window status, generating regular expressions for
symbol matching, etc.).

* A new major Vocola mode for Emacs that provides syntax coloring.

* Code for making Emacs and xterms work correctly with Dragon's
correction and sending non-ASCII characters code.

* A lot of other code to enable or enhance voice control:

  * 10,000 lines of Vocola code, providing thousands of voice commands
  * over 1000 "new" vocabulary words, some with formatting properties
  * code for generating DNS and Vocola lists
    numbers, URLs from Firefox, emails extracted from Outlook and LDAP,
    directories and files from filesystem scans, email folders
  * elisp code (elisp is the language Emacs uses) including:
    * changes to ace-jump to implement fast on-screen jumps
    * line numbers modulo 100
    * leap, which allows moving to the next occurrence of a pattern intelligently
    * elastic space, which types a space only if there is not already one there
    * Vi's start-word
    * selected code templates
    * rename file and buffer
    * moving by fragments of camel case words (e.g., Case in CamelCaseWord)
  * AutoHotkey scripts for:
    relocating correct that dialogue, volume control, killing Firefox
    plug-in containers, asynchronous message display
  * driving Dragon's vocabulary GUI to import words and their properties
  * converting English descriptions of identifiers to identifiers (e.g.,
    "monster memory map" -> "mmap")
  * VBA macros for Excel, Word (moving charts, moving by sane word boundaries)
  * moving by sane word boundaries by "peeking" at application text by
    the clipboard
  * logging of utterances, analysis therein
  * generating a corpus of words from my email and papers for writing analysis
  * keeping commands and vocabularies synchronized across multiple machines
  * converting browser bookmark list to a webpage of accelerators
