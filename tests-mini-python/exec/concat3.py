def f(i, j):
    if i >= j:
        return []
    return [i] + f(i+1, j)

print([] + [])
print([1,2] + [3,4,5])
print(f(4, 7))
