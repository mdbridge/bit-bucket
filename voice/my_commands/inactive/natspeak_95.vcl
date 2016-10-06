Word(written, spoken) := WaitForWindow("Vocabulary Editor") Wait(50)
                         {alt+w} $written Wait(50) {tab} $spoken {tab}{space};

Properties() := Wait(50) {alt+p} WaitForWindow("Word Properties");


## 
## Manual fixes for DNS 9.5
## 

SetAlternate(normal, alternative) := Word($normal, "") Properties()
                                     {alt+u}+ {tab} $alternative {enter}; 


set vocabulary properties for 9.5 =
    SetAlternate(e-mail,    email)
    SetAlternate(e-mailed,  emailed)
    SetAlternate(e-mailer,  emailer)
    SetAlternate(e-mailers, emailers)
    SetAlternate(e-mailing, emailing)
    SetAlternate(e-mails,   emails)
    ;
