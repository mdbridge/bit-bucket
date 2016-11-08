### 
### Dealing with Latex
### 

include "gnu.vch";


List(indent) := $indent \begin{{}itemize} {enter}
		$indent "  \item "        {enter}
		$indent "  \item "        {enter}
		$indent "  \item "        {enter}
                $indent \end{{}itemize}   {enter}
                {up_4} {end}
		;

begin itemize [1..9] = List( When($1,Repeat($1,"  ")) );


latex footnote = "\footnote{{}" {enter} %{enter} %{enter} } {up}{home}{ctrl+o};


# remember "double space periods" in gnu_spacing.vcl



<style> := ( emphasis='\emph{' | bold='\textbf{' | small caps='\textsc{'
	   | underline='\underline{'
	   | typewriter='\texttt{' | register=YankFromRegister(e) '{' );

<style> word [0..10] = $1 {ctrl+u} When($2,$2,1){esc}f "}" 
    If(Eval(When($2,$2,1) '==0'),{left});

open <style> = $1;



Align(regexp) := Do2(align-regexp, $regexp);

  # meant for tables with ... & ... & ... & ... \\
  # does not correctly handle \multicolumn
align table <r> comma <r> = 
    LineMod($1) Mark() LineMod($2)
    {ctrl+u} Do(align-regexp) "&"{enter} {enter} {backspace}2{enter}
        y
    LineMod($1) Mark() LineMod($2)
    Align(\\\\);
