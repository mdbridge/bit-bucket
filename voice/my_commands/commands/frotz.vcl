### 
### Voice commands for Infocom games
### 

$set MaximumCommands 4;


<direction> := ( north | east | south | west | soar | down 
	       | Northeast | Northwest | Southeast | Southwest);

<direction> = $1{enter};


look = look{enter};


Zed = z{Enter};

Again = g{Enter};

inventory  = i{Enter};

scrollback = {Ctrl+l};


Close   <_anything> = close  {Space} $1{Enter};
Drop    <_anything> = drop   {Space} $1{Enter};
Examine <_anything> = x      {Space} $1{Enter};
Get     <_anything> = get    {Space} $1{Enter};
Open    <_anything> = open   {Space} $1{Enter};
Unlock  <_anything> = unlock {Space} $1{Enter};
