
# N-queens

def abs(x):
    if x < 0: return -x
    else: return x

def check(b, i, n):
    for j in list(range(i)):
        if b[i] == b[j] or abs(b[i]-b[j]) == abs(i-j):
            return False
    return True

def count(b, i, n):
    if i == n: return 1
    c = 0
    for x in list(range(n)):
        b[i] = x
        if check(b, i, n):
            c = c + count(b, i+1, n)
    return c

def q(n):
    return count(list(range(n)), 0, n)

for n in list(range(9)):
    print(q(n))
