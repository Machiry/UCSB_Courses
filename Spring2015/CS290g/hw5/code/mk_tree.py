def chain(tree,x):
    c = [x]
    while x!=1:
        x=tree[x]
        c.append(x)
    return c[::-1]

tree = {1:None} 
leaves = [1]
levels = 10
for _ in range(levels):
    newleaves = []
    for m in leaves:
        for i in chain(tree,m):
            if i+m not in tree:
                tree[i+m] = m
                newleaves.append(i+m)
    leaves = newleaves

print 143
par = tree[143]
while par:
    print par
    par = tree[par]

