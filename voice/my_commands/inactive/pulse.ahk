;include "AutoHotkey.vch";
;
;  # hold given key down for $milliseconds:
;Pulse(key, milliseconds) := AutoHotkey2(pulse, $key, $milliseconds);

Send {%1% down}
Sleep %2%
Send {%1% up}
