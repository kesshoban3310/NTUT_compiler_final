def fact(n):
    if n <= 1:
        return 1
    return n * fact(n - 1)

def factimp(n):
    f = 1
    for x in list(range(n)):
        f = f * (x+1)
    return f

print(fact(10))
print(factimp(10))
