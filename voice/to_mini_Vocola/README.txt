    Code for converting my Vocola 2 commands to the mini Vocola format for
use with my mini Vocola compiler for converting to Advanced Scripting.


    We apply a modified version of the Vocola 2.8 Python compiler to
transform the source.  Changes:

  * inlines all include files, removing the include statement entirely
    * hard wires COMPUTERNAME to mdl for now

  ! omits context statements, ignores command sequence commands and
    other $set directives

  * inlines all user function calls removing any issues with differing
    function call semantics

  * pulls out references: "a$1 " -> "a" $1 " "


  ! optional terms are made not optional

  * _'s in list names are changed to --'s

  * does not transform Eval to EvalTemplate

  * outputs to .mv rather than _vcl.py files

  * pass input_name (filename being compiled) to emit_output, but not
    currently using it

  * lists are written out to lists/<name>.list, skipping alternatives
    that are disabled (more below)
    ! WARNING: no check is made as to whether or not the same list name
      in different files is used with different contents

  * lists that have no non-disabled alternatives become just (nothing)
    and commands/alternatives that reference them become disabled

  ! ranges are treated as lists of name <from>to<to>, with numbers (not
    spelled out) as values

  * in-line lists are converted to normal list via gensym (zzz1, zzz2, etc.)

  ! alternatives/commands containing <_anything> are disabled

  * replaces "String." in result so String.Left becomes Left, etc.

  ! non-word actions in alternatives are silently dropped


    Modified Mini-Vocola compiler to kludge { _ } notation (*all* _s
converted to spaces), added a Special.Split extension, Special.EQ
function.

    Replaced string.vch and control.vch with correct definitions for
Mini Vocola, kludged switch.vch to use AutoHotkey for now.

    Major functionality lost due to missing extensions: Variable,
switching with error return.  Affects range commands, switching
Windows.  Also don't have WaitForWindow as that isn't supported for new
Advanced Scripting commands; affects loading vocabulary properties.


2/2/2013: Tried all? of Win32Pad commands.  Everything works except
'find file' (WaitForWindow) and commands using underscores (key _, leap
_, etc.).


2/2/2013: Tried sample of 'emacs' commands.  Failing includes:
  save as PDF            (Variable.Set)
  set/reset <color> area (Variable.[Re]Set, Window.ID)
  most range commands    (Variable.Set/Get; could be kludged easily)
  destroy next rest      (optional word now required)
  set <color> buffer     (overlap of 2 different lists with same name)
  leap fails             (all _'s being converted to spaces (uses {ctrl+_}))
    otherwise works

  complaints about bad tube list value for work email

3/19/2016: updated original_extensions.csv
    issues with new If, When built-ins, optional lists & ranges
