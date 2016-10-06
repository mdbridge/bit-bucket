### 
### Gnuemacs commands for filling text
### 

include "gnu.vch";


  # a character not a whitespace character:
NWS() := "[^ {ctrl+q}{tab}{ctrl+q}{ctrl+j}]";


## 
## Built-in Emacs filling:
## 

justify        = {Esc}q;
justify before = SaveExcursion(LeapRegexRaw(u, NWS())  {Esc}q);

# see justify <range>



## 
## Filling/indenting paragraphs:
## 

  # call with point at start of a paragraph:
SlideHere() :=
    LeapRegexRaw(D, NWS()) {home}  # skip leading blank line if any
    {esc}q {home}
    "    "
      # ~cap-a-letter:  sets mark <<<>>>
    Mark() {right} Do(capitalize-region)
    {esc}q
    {esc}};

NextSlide() :=
      # move start of current paragraph if in one else start of next paragraph:
    {home} LeapRegexRaw(D, NWS()) {esc}{{}  
    SlideHere();


          slide before           =                {esc}{{} NextSlide();
          slide (here|paragraph) = SaveExcursion( {esc}{{} NextSlide() );

          slide paragraph 1..20  =  Repeat($1, NextSlide());
<row> <r> slide paragraph        = LineMod($2) NextSlide();

play paragraph = Beep();  # confused for slide paragraph



## 
## Experiments: <<<>>>
## 

          slide graph = SaveExcursion( {esc}{{} NextSlide() );

          slide graph 1..20  =  Repeat($1, NextSlide());
<row> <r> slide graph        = LineMod($2) NextSlide();


  # use no fill prefix at all:			
left justify line =
     Elisp("(setq adaptive-fill-mode nil)")
	SaveExcursion(
	  {home} Mark() {end}
	  Do(fill-region)
        )
     Elisp("(setq adaptive-fill-mode t)")
     ;

justify comment line =
	SaveExcursion(
	  {home} Mark()
	  LeapRegex(D+, "[ ]*[;#/]*[ ]") {ctrl+x}.
	  {end}
	  Do(fill-region)
	  {home} {ctrl+x}.
        );
