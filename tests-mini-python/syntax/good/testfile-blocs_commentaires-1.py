def fonction1(x):
    return [ x ]

def fonction2(x, y):
    return fonction1(y + x)

def fonction3():
    # Une fonction sans argument
    x = list(range(3))
    y = list(range(4))
    for i in y:
        for j in x:
            print(fonction2(x,y))

    # retour de bloc
    # et commentaire sur plusieurs lignes

    return []


print(fonction3())
