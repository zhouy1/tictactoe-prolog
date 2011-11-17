def board(x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, p, q):
    array=[]
    N=['X','Y','Z','W']
    for n in range(0,4):
        for i in range(0,4):
            for j in range(0,4):
                if ((z1 == n and x1 == j and y1 == i) or \
                    (z2 == n and x2 == j and y2 == i) or \
                    (z3 == n and x3 == j and y3 == i)):
                    array.append(q)
                elif (z4 == n and x4 == j and y4 == i):
                    array.append(p)
                else:
                    array.append('%s%d%d' % (N[n],i+1,j+1))
    return '/'.join(array)

def win_block(f,q):
	for z in range(0,4):
		for y in range(0,4):
			print '%s(' % f
			print board(1,y,z,2,y,z,3,y,z,0,y,z,'_',q) + ','
			print '%d, %d, %d,' % (0, y, z)
			print board(1,y,z,2,y,z,3,y,z,0,y,z,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(0,y,z,2,y,z,3,y,z,1,y,z,'_',q) + ','
			print '%d, %d, %d,' % (1, y, z)
			print board(0,y,z,2,y,z,3,y,z,1,y,z,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(0,y,z,1,y,z,3,y,z,2,y,z,'_',q) + ','
			print '%d, %d, %d,' % (2, y, z)
			print board(0,y,z,1,y,z,3,y,z,2,y,z,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(0,y,z,1,y,z,2,y,z,3,y,z,'_',q) + ','
			print '%d, %d, %d,' % (3, y, z)
			print board(0,y,z,1,y,z,2,y,z,3,y,z,'x',q)
			print ').'
			print ''

	for z in range(0,4):
		for x in range(0,4):
			print '%s(' % f
			print board(x,1,z,x,2,z,x,3,z,x,0,z,'_',q) + ','
			print '%d, %d, %d,' % (x, 0, z)
			print board(x,1,z,x,2,z,x,3,z,x,0,z,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(x,0,z,x,2,z,x,3,z,x,1,z,'_',q) + ','
			print '%d, %d, %d,' % (x, 1, z)
			print board(x,0,z,x,2,z,x,3,z,x,1,z,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(x,0,z,x,1,z,x,3,z,x,2,z,'_',q) + ','
			print '%d, %d, %d,' % (x, 2, z)
			print board(x,0,z,x,1,z,x,3,z,x,2,z,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(x,0,z,x,1,z,x,2,z,x,3,z,'_',q) + ','
			print '%d, %d, %d,' % (x, 3, z)
			print board(x,0,z,x,1,z,x,2,z,x,3,z,'x',q)
			print ').'
			print ''

	for y in range(0,4):
		for x in range(0,4):
			print '%s(' % f
			print board(x,y,1,x,y,2,x,y,3,x,y,0,'_',q) + ','
			print '%d, %d, %d,' % (x, y, 0)
			print board(x,y,1,x,y,2,x,y,3,x,y,0,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(x,y,0,x,y,2,x,y,3,x,y,1,'_',q) + ','
			print '%d, %d, %d,' % (x, y, 1)
			print board(x,y,0,x,y,2,x,y,3,x,y,1,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(x,y,0,x,y,1,x,y,3,x,y,2,'_',q) + ','
			print '%d, %d, %d,' % (x, y, 2)
			print board(x,y,0,x,y,1,x,y,3,x,y,2,'x',q)
			print ').'
			print ''
			print '%s(' % f
			print board(x,y,0,x,y,1,x,y,2,x,y,3,'_',q) + ','
			print '%d, %d, %d,' % (x, y, 3)
			print board(x,y,0,x,y,1,x,y,2,x,y,3,'x',q)
			print ').'
			print ''

	for y in range(0,4):
		print '%s(' % f
		print board(1,y,1,2,y,2,3,y,3,0,y,0,'_',q) + ','
		print '%d, %d, %d,' % (0,y,0)
		print board(1,y,1,2,y,2,3,y,3,0,y,0,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(0,y,0,2,y,2,3,y,3,1,y,1,'_',q) + ','
		print '%d, %d, %d,' % (1,y,1)
		print board(0,y,0,2,y,2,x,y,3,1,y,1,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(0,y,0,1,y,1,3,y,3,2,y,2,'_',q) + ','
		print '%d, %d, %d,' % (2,y,2)
		print board(0,y,0,1,y,1,3,y,3,2,y,2,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(0,y,0,1,y,1,2,y,2,3,y,3,'_',q) + ','
		print '%d, %d, %d,' % (3,y,3)
		print board(0,y,0,1,y,1,2,y,2,3,y,3,'x',q)
		print ').'
		print ''

		print '%s(' % f
		print board(y,1,1,y,2,2,y,3,3,y,0,0,'_',q) + ','
		print '%d, %d, %d,' % (y,0,0)
		print board(y,1,1,y,2,2,y,3,3,y,0,0,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(y,0,0,y,2,2,y,3,3,y,1,1,'_',q) + ','
		print '%d, %d, %d,' % (y,1,1)
		print board(y,0,0,y,2,2,y,3,3,y,1,1,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(y,0,0,y,1,1,y,3,3,y,2,2,'_',q) + ','
		print '%d, %d, %d,' % (y,2,2)
		print board(y,0,0,y,1,1,y,3,3,y,2,2,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(y,0,0,y,1,1,y,2,2,y,3,3,'_',q) + ','
		print '%d, %d, %d,' % (y,3,3)
		print board(y,0,0,y,1,1,y,2,2,y,3,3,'x',q)
		print ').'
		print ''

		print '%s(' % f
		print board(1,1,y,2,2,y,3,3,y,0,0,y,'_',q) + ','
		print '%d, %d, %d,' % (0,0,y)
		print board(1,1,y,2,2,y,3,3,y,0,0,y,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(0,0,y,2,2,y,3,3,y,1,1,y,'_',q) + ','
		print '%d, %d, %d,' % (1,1,y)
		print board(0,0,y,2,2,y,3,3,y,1,1,y,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(0,0,y,1,1,y,3,3,y,2,2,y,'_',q) + ','
		print '%d, %d, %d,' % (2,2,y)
		print board(0,0,y,1,1,y,3,3,y,2,2,y,'x',q)
		print ').'
		print ''
		print '%s(' % f
		print board(0,0,y,1,1,y,2,2,y,3,3,y,'_',q) + ','
		print '%d, %d, %d,' % (3,3,y)
		print board(0,0,y,1,1,y,2,2,y,3,3,y,'x',q)
		print ').'
		print ''

	print '%s(' % f
	print board(1,1,1,2,2,2,3,3,3,0,0,0,'_',q) + ','
	print '%d, %d, %d,' % (0,0,0)
	print board(1,1,1,2,2,2,3,3,3,0,0,0,'x',q)
	print ').'
	print ''
	print '%s(' % f
	print board(0,0,0,2,2,2,3,3,3,1,1,1,'_',q) + ','
	print '%d, %d, %d,' % (1,1,1)
	print board(0,0,0,2,2,2,3,3,3,1,1,1,'x',q)
	print ').'
	print ''
	print '%s(' % f
	print board(0,0,0,1,1,1,3,3,3,2,2,2,'_',q) + ','
	print '%d, %d, %d,' % (2,2,2)
	print board(0,0,0,1,1,1,3,3,3,2,2,2,'x',q)
	print ').'
	print ''
	print '%s(' % f
	print board(0,0,0,1,1,1,2,2,2,3,3,3,'_',q) + ','
	print '%d, %d, %d,' % (3,3,3)
	print board(0,0,0,1,1,1,2,2,2,3,3,3,'x',q)
	print ').'
	print ''

def win():
    win_block('win', 'x')

def block():
    win_block('block', 'o')

win()
block()

# TODO: fork
# TODO: block opponent fork
# TODO: center
# TODO: opposite corner
# TODO: empty corner
# TODO: empty side


