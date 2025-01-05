def f(x,y,z,t): # commentaire autorisé ici
    if x >= y:
        return t
    else:
        for i in list(range(x)):
            x = x+i

    print(x)
    return x


print(f(1,2,3,5))


