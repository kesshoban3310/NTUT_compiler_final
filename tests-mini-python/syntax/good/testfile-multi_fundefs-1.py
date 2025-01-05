def plus (x, y):
    return x + y

def moins (x, y):
    return x - y

def mult (x, y):
    return x * y

def div (x, y):
    return x // y

def modulo (x, y):
    return x % y


x = 3
y = 4
z = 5

print(x % y + z - 5 * y + -1 // 2 == plus(moins(plus(modulo(x,y), z), mult(5,y)), div(-1,2)))
