    Code for converting my Vocola 2 commands to the Vocola 3 format for
use with Vocola 3.


    We apply a modified version of the Vocola 2.8 Python compiler to
transform the source.  Changes:

  * inlines all include files, removing the include statement entirely
    * hard wires COMPUTERNAME to mdl for now

  ! omits context statements

  * unparsing code implicitly fixes <function> (...) := Vocola 3 syntax
    error

  * all resulting string constant actions are quoted so no issues with
    <>'s now must be quoted

  * converted /'s in spoken forms to slash's to avoid SAPI bug

  ! converted -'s in spoken forms to space's because WSR does not
    recognize hyphenated spoken forms.  This may screw up <prn>.

  * does not transform Eval to EvalTemplate

  * inlines all user function calls removing any issues with differing
    function call semantics
    * except for EQ function

  * Comments out commands using DNS features/extensions not currently
    available with Vocola 3; ditto list entries using the same
    ! Beep, MsgBoxConfirm, Unimacro, ShiftKey, HTMLHelp, TTSPlayString,
      SendDragonKeys, AppBringUp, EvalTemplate

  * replace strings.vch with code to use Vocola 3 string extension
    rather than Eval

  * replace control.vch with code to use WSR-specific Eval expression for EQ

  * Convert Clipboard.{Get,Set} to Clipboard.{GetText,SetText}

  * added noise.vcl to help control noise words

  * manually tweaked button codes in _mouse.vcl to deal with Vocola 3's
    inability to deal with swapped mouse buttons
  * converted "ButtonClick([12], " likewise to flip mouse buttons

  * replaced switch.vch to avoid using Window.SwitchTo, Window.ID:
    * OldWindow hardwired to "foil emacs"
    * "blue area"->"startup xterm", "black area"->"xterm whiskey"
    * switching done via AutoHotkey script; assumes always succeeds
      * script does not wait so I had to kludge it with a explicit Wait


====================  cut here  ====================

No variable.reset, default argument for variable.get
   Calling Variable.Get with two arguments is main remaining Vocola 3
   compilation error

EvalTemplate currently only used in PowerPoint and disabled_win32pad for
fairly complicated math.

SendDragonKeys only used by _test for testing dropping bug

currently no way to create applications whose macros only create if
switch fails

> <pick> := (('pick' = '{shift}' Library.Main.HearCommand(Press,enter) | 'go pick' = '{shift}' | 'push pick' = '{shift}{ctrl+enter}' | 'tab pick' = '{shift}{ctrl+shift+enter}' | 'window pick' = '{shift}{shift+enter}'));

String is missing Mid[3]; I coped via Eval arithmetic

