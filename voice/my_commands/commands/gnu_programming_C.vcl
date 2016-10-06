### 
### Voice commands for programming in C and C++:
### 
###   See also gnu_programming_C_style.vcl
###            gnu_programming_C_include.vcl
### 

include "gnu.vch";

BackTil(target) := Leap(u, $target);


## 
## Additional C-specific statement abbreviations:
## 

<printf> := ( print F='printf(' | standard error printf='fprintf(stderr, ' );
<type>   := ( integer=%d | C string=%s | long=%ld | long long=%lld 
	    | pointer=%p | hex=0x%x);

<printf> statement = {tab} {shift+space} $1 '"\n");'             BackTil("\");
<printf> <type>    = {tab} {shift+space} $1 '"$2\n", );'         BackTil(")");
<printf> string    = {tab} {shift+space} $1 '"%s\n", .c_str());' BackTil(".");


add include guards = {ctrl+u} Do(mdl-copy-buffer-filename) Wait(0)
  Replace(
    {esc}<
    "#ifndef %{enter}#define %{enter}{enter}"
    {ctrl+space}{esc}>
    "{enter}#endif /* % */{enter}"
    {ctrl+x}{ctrl+x}, %, 
    _ Upper(Replace(Clipboard.Get(),".","_")) _);


BlockPair() :=  "{{}" {enter_2} "}" {tab}{up}{tab};

short C main routine = "int main(void) " BlockPair();
short [plus] main routine = "int main() " BlockPair();
main routine = "int main(int argc, char* argv[]) " BlockPair();



## 
## Inserting debugging statements:
## 

mdl break 1..99 = 
   {home}{ctrl+o} '{tab}std::cout << "[mdl break $1]" << std::endl;';



## 
## Additional C++-specific statement abbreviations:
## 

  # assumes class name is on clipboard
disable copy and assignment =
    "private:" {enter}{tab}
    "{ctrl+y}(const {ctrl+y}&);              // disable copying"    {enter}{tab}
    "{ctrl+y}& operator=(const {ctrl+y}&);   // disable assignment" {enter};
new disable copy and assignment =
    "{ctrl+y}(const {ctrl+y}&)            = delete;"    {tab}{enter}{tab}
    "{ctrl+y}& operator=(const {ctrl+y}&) = delete;" {enter};



  # assumes iterator expression in clipboard:
(constant iterator=const_iterator | iterator) loop = 
    "for (::$1 it={ctrl+y}.begin(),itend={ctrl+y}.end(); it!=itend; ++it)" 
    BackTil('::');
reverse (constant iterator= const_iterator | iterator) loop = 
    "for (::$1 it={ctrl+y}.rbegin(),itend={ctrl+y}.rend(); it!=itend; ++it)" 
    BackTil('::');



## 
## E Browse:
## 

# just visit a BROWSE file to start
# gives a tree browsing buffer; enter on a line to go to that class
# can use mouse 3 to browse by click on names or empty space in resulting buffer


#
# Looking up a member:
#

  # show means switch to a different window then go to target
<lookup> := (go="" | show="4");
<kind>   := (definition=f | declaration=F | interface=F);

  # lookup name under point:
<lookup> <kind> = {ctrl+c}{ctrl+m}$1 $2 {enter};
  # Lookup name of function [defined/declared on given line]:
<lookup> function [<my0to99>] <kind> = 
    When($2,LineMod($2)) {home} Leap(D,'(') {ctrl+c}{ctrl+m}$1 $3 {enter};

<kind> monster <_anything> = 
    {ctrl+c}{ctrl+m}$1 Mark() {home} {ctrl+w} "monster $2"
    Mark() {home} ToggleRegion() {enter};

  # choosing which completion:
E browse <my0to99> = {ctrl+u}-1{ctrl+x}o LineMod($1) {enter};

  # move within stack of positions (saves position each time do a lookup)
E browse (back=-|forward=+) = {ctrl+c}{ctrl+m} $1;



## 
## E Browse experiments:
## 

# you can also produce buffer showing all the members of a given class
# of a given kind
#   use long form so selecting easy
#   (no commands written for this yet)

  # list all members in current file:
E browse list [members] = {ctrl+u} Do(mdl-copy-buffer-filename)
                          {ctrl+c}{ctrl+m} l {ctrl+y}{enter};


E browse apropos        = {ctrl+c}{ctrl+m}a;
E browse complete       = {ctrl+c}{ctrl+m}{tab};

  # prefix for experimentation:
E browse                = {ctrl+c}{ctrl+m};
