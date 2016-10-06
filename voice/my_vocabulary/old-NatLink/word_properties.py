###
### Word_properties:
### 
###   Functions for converting to and from human readable word property
###   descriptions.
### 

import sys



##
## Full definition of word property flags from NatLink documentation:
## 

USER_ADDED = 0x00000001

word_flags = [
    [ 0x00000001, "Word was added by the user" ],
    [ 0x00000002, "Internal use only" ],
    [ 0x00000004, "Internal use only" ],
    [ 0x00000008, "Word can not be deleted" ],
    [ 0x00000010, "Normally capitalize the next word (like period)" ],
    [ 0x00000020, "Always capitalize the next word (like Cap Next)" ],
    [ 0x00000040, "Uppercase the next word (like All Caps Next)" ],
    [ 0x00000080, "Lowercase the next word (like No Caps Next)" ],
    [ 0x00000100, "No space following this word (like left paren)" ],
    [ 0x00000200, "Two spaces following this word (like period)" ],
    [ 0x00000400, "No spaces between words with this flag set"
                  " (like with numbers)" ],
    [ 0x00000800, "Turn capitalization mode on (like Caps On)" ],
    [ 0x00001000, "Turn uppercase mode on (like All Caps On)" ],
    [ 0x00002000, "Turn lowercase mode on (like No Caps On)" ],
    [ 0x00004000, "Turn off spacing between words (like No Space On)" ],
    [ 0x00008000, "Restore normal spacing (like No Space Off)" ],
    [ 0x00010000, "Internal use only" ],
    [ 0x00020000, "Suppress after a word which ends in a period"
                  " (like period after elipsis)"],
    [ 0x00040000, "Do not apply formatting to this word (like Cap)" ],
    [ 0x00080000, "Do not reset the spacing state (like Cap)" ],
    [ 0x00100000, "Do not reset the capitalization state (like close quote)" ],
    [ 0x00200000, "No space preceeding this word (like comma)" ],
    [ 0x00400000, "Restore normal capitalization (like Caps Off)" ],
    [ 0x00800000, "Follow this word with one new line characters"
                  " (like New-Line)" ],
    [ 0x01000000, "Follow this word with two new line characters"
                  " (like New-Paragraph)" ],
    [ 0x02000000, "Do not capitalize this word in a title (like and)" ],
    [ 0x04000000, "Internal use only" ],
    [ 0x08000000, "Add an extra space following this word (like space-bar)" ],
    [ 0x10000000, "Internal use only" ],
    [ 0x20000000, "Internal use only" ],
    [ 0x40000000, "Word was added by the vocabulary builder." ]
    ]


def unpack_flags(flags):
    annotated_flags = []
    for f in word_flags:
        if flags & f[0]:
            annotated_flags += [f]

    return annotated_flags

    

##
## Classes of word properties:
## 

addition_properties = USER_ADDED | 0x40000000

internal_properties = 0x00000002 | 0x00000004 | 0x00010000 | 0x04000000 \
                    | 0x10000000 | 0x20000000

other_properties    = 0x7fffffff ^ (addition_properties|internal_properties)


  # properties we should not normally reset when modifying a word's properties:
retained_properties = addition_properties | internal_properties | 0x00000008



##
##
## 

property_names = [
    [ "tight",        0x00000100 | 0x00200000 ],
    [ ".",            0x00000100 | 0x00200000 ],
    [ "prefix",       0x00000100 ],
    [ "suffix",       0x00200000 ],

    [ "keep-caps",    0x00100000 ],
    [ "keep-spacing", 0x00080000 ],

    [ "undeletable",  0x00000008 ],


    [ "p",            0x00000100 ],
    [ "s",            0x00200000 ]
    ]


def unpack_properties(flags):
    properties = []

    for name_definition in property_names:
        if (flags&name_definition[1]) == name_definition[1]:
            properties += [name_definition[0]]
            flags ^= name_definition[1]

    if flags != 0:
        properties += ["x%08x" % flags]

    return properties



def apply_properties(properties, old_flags):
    flags = old_flags & retained_properties
    
    for property in properties:
        found = False
        for name in property_names:
            if name[0] == property:
                flags |= name[1]
                found = True
        if len(property)>0 and property[0] == "x":
            flags |= int(property[1:], 16)
            found = True
        if not found:
            print >> sys.stderr, "Error: unknown property '%s'" % property

    return flags
