### 
### Voice commands for editing HTML
### 
###   Filename: HTML.vch (include file)
### 

include "string.vch";

  # Input:  string $s containing at most one %, $s contains no braces
  # Output: types $s without any %'s except that it replaces @'s with
  #         enter's then moves cursor back to where % was if any was present
Position(s) := Replace( Replace($s, "%", ""), "@", {enter})
               '{left ' Len( Split("$s%", "%", 1) ) };


## 
## Commands for starting an [X]HTML file:
## 

XML Tag                           = '<?xml version="1.0" encoding="UTF-8"?>';

DocType(type, tail) :=
    '<!DOCTYPE html PUBLIC "-//W3C//DTD $type//EN"$tail>';

Document Type HTML 4             = DocType("HTML 4.0 Transitional", "");

Document Type XHTML Strict       = 
    DocType("XHTML 1.0 Strict",       {enter}{enter}
        '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"');

Document Type XHTML Transitional = 
    DocType("XHTML 1.0 Transitional", {enter}{enter}
        '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"');

Document Type XHTML FrameSet     = 
    DocType("XHTML 1.0 Frameset",     {enter}{enter}
        '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"');

HTML Root                        = 
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">';

HTML Skeleton               = 
    Position(<html>           @
             <head>           @
             <title>%</title> @
             </head>          @
             <body>           @
             </body>          @
             </html>);

HTML Skeleton Without Title = 
    Position(<html>           @
             <head>           @
             </head>          @
             <body>           @
             </body>          @
             </html>);


## 
## Inline tags (e.g., <tag></tag> with the cursor left between the two tags):
## 

<inline> := (
    title                      |
    body                       |
    Heading 1         = h1     |
    Heading 2         = h2     |
    Heading 3         = h3     |
    Heading 4         = h4     |
    Heading 5         = h5     |
    Heading 6         = h6     |
    
    No Break          = nobr   |
    
    Preformatted Text = pre    |
    Emphasize         = em     |
    strong                     |
    Program Code      = code   |
    Sample Text       = sample |
    Keyboard          = kbd    |
    Variable          = var    |
    Definition        = dfn    |
    Quotation         = cite   |
    
    Bold              = b      |
    blink                      |
    Italic            = i      | Italics    = i  | 
    Underline         = u      |
    Strike through    = strike |
    Subscript         = sub    |
    Superscript       = sup    |
    Large             = big    | big             |
    small                      |
    
    font                       |
    Style             = STYLE  |
    
    List Item         = li     |
    menu                       |
    Directory List    = dir    | Directory = dir | 
    Definition List   = dl     |
    Definition Term   = dt     |
    Data Definition   = dd     |
    
    marquee                    |
    
    caption                    |
    Table Row         = tr     |
    Table Heading     = th     |
    Table Detail      = td     |
    col                        | Column    = col 
);

<inline>                 Tag = Position(<$1>%</$1>);
<inline> Begin           Tag = <$1>;
<inline> (End | Closing) Tag = </$1>;




HTML Tag                          = '<html>{Enter}{Enter}</html>{Left_8}';
HTML Begin Tag                    = '<html>{Enter}';
HTML (End | Closing) Tag          = '</html>{Enter}';

Head Tag                          = '<head></head>{Left_7}';
Head Begin Tag                    = '<head>';
Head (End | Closing) Tag          = '</head>{Enter}';

Meta Tag                          = '<meta http-equiv="refresh" content="">';

Comment Tag                       = '<! ->';

Horizontal (Rule | Line) [Tag]    = '<hr>{Enter}';
#Horizontal (Rule | Line) [Tag]    = '<hr />{Enter}';
[Line] Break Tag                  = '<br>';
#[Line] Break Tag                  = '<br />';
Paragraph Tag                     = '<p>{Enter}';
#Paragraph Tag                     = '<p><p />{Enter}';

Space Escape Code                 = ' ';
1..50 Space Escape Codes          = Repeat($1, ' ');
Ampersand Escape Code             = '&';
Apostrophe Escape Code            = "'";


Center Tag                        = '<center>{Enter}{Enter}</center>{Left_10}';
Div Tag                           = '<div align="">{Enter}{Enter}</div>{Left_7}';
Span Tag                          = '<span>{Enter}{Enter}</span>{Left_8}';
Map Tag                           = '<MAP NAME="">{Enter}{Enter}</MAP>{Left_7}';
Area Tag                          = '<area shape="rect" coords="" href="" onClick="" alt="">{Left_20}{Left_8}';
#Area Tag                          = '<area shape="rect" coords="" href="" onClick="" alt="" />{Left_20}{Left_10}';

Unordered List Tag                = '<ul type="disc|circle|square">{Enter}{Enter}</ul>{Left_20}{Left_6}';
Ordered List Tag                  = '<ol type="1">{Enter}{Enter}</ol>{Left_6}';
Anchor Tag                        = '<a href=""></a>{Left_6}';
Anchor Name Tag                   = '<a name=""></a>{Left_6}';
Image Tag                         = '<img src="" alt="">{Left_9}';
#Image Tag                         = '<img src="" alt="" />{Left_11}';

Align Attribute                   = 'align=""{Left_1}';
Background Color Attribute        = 'bgcolor=""{Left_1}';
Border Attribute                  = 'border=""{Left_1}';
Cell Padding Attribute            = 'cellpadding=""{Left_1}';  #Space between cell contents & edge of cell
Cell Spacing Attribute            = 'cellspacing=""{Left_1}';  #size of cell border between cells
Color Attribute                   = 'color=""{Left_1}';
Column Span Attribute             = 'colspan=""{Left_1}';
Columns Attribute                 = 'cols=""{Left_1}';
[Font] Face Attribute             = 'face=""{Left_1}';
Height Attribute                  = 'height=""{Left_1}';
Multiple Attribute                = 'multiple';
Ordered List Attribute Type       = 'type="1AaIi"{Left_6}';
Read-Only Attribute               = 'readonly';
Row Span Attribute                = 'rowspan=""{Left_1}';
Rows Attribute                    = 'rows=""{Left_1}';
Selected Attribute                = 'selected';
Size Attribute                    = 'size=""{Left_1}';
Source Attribute                  = 'src=""{Left_1}';
Unordered List Attribute Type     = 'type="disc|circle|square"{Left_19}';
Width Attribute                   = 'width=""{Left_1}';
Vertical Align Attribute          = 'valign="top|middle|bottom"{Left_18}';

On Focus Event                    = 'onfocus ="" {Left_2}';
On Blur Event                     = 'onblur="" {Left_2}';
On Click Event                    = 'onClick="" ';
On Mouse Over Event               = 'onMouseover="" ';
On Mouse Out Event                = 'onMouseout="" ';

Background Sound Tag              = '<bgsound src="">{Left_2}';
#Background Sound Tag              = '<bgsound src="" />{Left_4}';

Table Tag                         = '<table>{Enter}{Enter}</table>{Left_9}';
Table Border Tag                  = '<tableborder bgcolor="">{Left_2}';
Table Head Tag                    = '<thead>{Enter}{Enter}</thead>{Left_9}';
Table Body Tag                    = '<tbody>{Enter}{Enter}</tbody>{Left_9}';
Table Foot Tag                    = '<tfoot>{Enter}{Enter}</tfoot>{Left_9}';
(Col | Column) Group Tag          = '<colgroup>{Enter}{Enter}</colgroup>{Left_12}';
Address Tag                       = '<address>{Enter}{Enter}</address>{Left_11}';

#Form Tag                          = '<form>{Enter}{Enter}</form>{Left_8}';
Form Tag                          = '<form name="" action="" method="post">{Enter}{Enter}</FORM>{Left_20}{Left_15}';
Input Tag                         = '<input name="" type="" size="" maxlength="" value="">{Left_20}{Left_20}';
Text Tag                          = '<input name="" type="text" value="" size="" maxlength="">{Left_20}{Left_20}{Left_4}';
Password Tag                      = '<input name="" type="password" size="" maxlength="">{Left_20}{Left_19}';
Submit Button Tag                 = '<input name="" type="submit">{Left_16}';
Radio Button Tag                  = '<input name="" type="radio" value="">{Left_20}{Left_4}';
Checked Radio Button Tag          = '<input name="" type="radio" value="" checked>{Left_20}{Left_12}';
#Checked Radio Button Tag          = '<input name="" type="radio" value="" checked="checked">{Left_20}{Left_20}{Left_2}';
Checkbox Tag                      = '<input name="" type="checkbox" value="" checked>{Left_20}{Left_15}';
#Checkbox Tag                      = '<input name="" type="checkbox" value="" checked="checked">{Left_20}{Left_20}{Left_5}';
Text Area Tag                     = '<textarea name="" rows="" cols=""></textarea>{Left_20}{Left_9}';
Listbox Tag                       = '<select name="" multiple size="">{Enter}{Enter}</select>{Left_20}{Left_10}';
#Listbox Tag                       = '<select name="" multiple="multiple" size="">{Enter}{Enter}</select>{Left_20}{Left_20}{Left_1}';
Option Group Tag                  = '<optgroup label="">{Enter}{Enter}</optgroup>{Left_15}';
Option Tag                        = '<option value="">{Left_2}';
#Option Tag                        = '<option value="" />{Left_4}';

Frame Set Tag                     = '<frameset>{Enter}{Enter}</frameset>{Left_12}';
FrameSet Rows Tag                 = '<frameset rows="">{Enter}{Enter}</frameset>{Left_15}';
FrameSet (Cols | Columns) Tag     = '<frameset cols="">{Enter}{Enter}</frameset>{Left_15}';
Frame Tag                         = '<frame>{Enter}{Enter}</frame>{Left_9}';
