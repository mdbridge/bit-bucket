# Vocola function: Symbols.Monster
def regex_for_monster(text):
    words = text.split()
    return " *[-_]*".join([monster_word(x) for x in words])
def monster_word(word):
    word = word.lower()
    return word[0] + "".join([x+"?" for x in word[1:]])
