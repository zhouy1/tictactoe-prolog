def board(x, y, z, p):
    array=[]
    N=['X','Y','Z','W']
    for n in range(0,4):
        for i in range(0,4):
            for j in range(0,4):
                if (z == n and x == j and y == i):
                    array.append(p)
                else:
                    array.append('%s%d%d' % (N[n],i+1,j+1))
    return '/'.join(array)

for z in range(0,4):
    for y in range(0,4):
        for x in range(0,4):
            print 'mark('
            print board(x,y,z,'_') + ','
            print '%d, %d, %d, P,' % (x,y,z)
            print board(x,y,z,'P')
            print ').'
            print ''

