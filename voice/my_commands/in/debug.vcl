## 
## Inserting debug/assert statements:
## 

PLACE(before, after) := $before $after {left_ Len($after) };

       Insert(statement) := {ctrl+a}{ctrl+o} $statement {home}{down};
MonsterInsert(statement) := Insert($statement Mark() {home} ToggleRegion());


Wisconsin assert 0..9   = PLACE("w_assert$1(", ");");


debug out statement     = PLACE('DBGOUT1(<<"', '");');

debug include           = Insert('#include "w_debug.h" // mdl-break <<<>>>');

debug clipboard print   = 
    MonsterInsert('DBGOUT1(<<"{ctrl+y}: "<<{ctrl+y}); // mdl-break <<<>>>');

debug print <_anything> = 
    MonsterInsert('DBGOUT1(<<"$1: "<<$1); // mdl-break <<<>>>');

debug info  <_anything> = 
    MonsterInsert('DBGOUT1(<<"$1"); // mdl-break <<<>>>');




#
# use __FILE__ and the like?  "@ file:13"?
# use back to func start to get func name?
#

