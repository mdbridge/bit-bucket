export message =
      "{ctrl+x}h{Esc}w{ctrl+x}{ctrl+f}{ctrl+a}{ctrl+k}"
      "~/Tmp/l1{Enter}{Home}{ctrl+y}{Esc}y"
      "{ctrl+space}{Esc}>{ctrl+w}"
      "{ctrl+x}{ctrl+s}{ctrl+x}k{Enter}";



# 
# These commands fixup a DNS bug:
# 

apply capitalization = SaveExcursion({end} Leap(u, "cap ")      {Del_4} {esc}c) 
                          {right_4};
apply upper          = SaveExcursion({end} Leap(u, "all-caps ") {Del_9} {esc}u);

shell apply capitalization = {end} Leap(u, "cap ")      {Del_4} {esc}c;
shell apply upper          = {end} Leap(u, "all-caps ") {Del_9} {esc}u;



highlight frequent words = Do(highlight-regexp)
"\b\(the\|of\|to\|and\|a\|in\|is\|it\|you\|that\|he\|was\|for\|on\|are\|with\|as\|I\|his\|they\|be\|at\|one\|have\|this\|from\|or\|had\|by\|hot\|but\|some\|what\|there\|we\|can\|out\|other\|were\|all\|your\|when\|up\|use\|word\|how\|said\|an\|each\|she\|which\|do\|their\|time\|if\|will\|way\|about\|many\|then\|them\|would\|write\|like\|so\|these\|her\|long\|make\|thing\|see\|him\|two\|has\|look\|more\|day\|could\|go\|come\|did\|my\|sound\|no\|most\|number\|who\|over\|know\|water\|than\|call\|first\|people\|may\|down\|side\|been\|now\|find\)\b"
    {enter};
