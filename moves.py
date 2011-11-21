print ':- module(moves,'
print '        [ put/4,                 % Put a piece in the board'
print '          is_empty/2             % Check if the position is empty'
print '        ]).'
print ''
print '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
print '%%%'
print '%%%     Moves'
print '%%%'
print '%%%'
print '%%%     The h_func calculates the heuristics function for a'
print '%%%     given position of the board.'
print '%%%'
print '%%%         put(+Board, +[Z,Y,X], +Piece, -NewBoard)'
print '%%%'
print '%%%         where Board is the current board state'
print '%%%               [Z,Y,X] is the board position'
print '%%%               Piece is the piece of current player (x/o)'
print '%%%               NewBoard is the board state'
print ''

def draw(x, y, z, p):
	N=['W','Z','Y','X']
	board=[]
	for k in range(0,4):
	    plane=[]
	    for j in range(0,4):
	        line=[]
	        for i in range(0,4):
	            if (z == 3-k and y == j and x == i):
	                line.append(" " + p + " ")
	            else:
	                line.append('%s%d%d' % (N[k],j,i))
	        plane.append("  " + " / ".join(line))
	    board.append(" /\n".join(plane))
	return " /\n\n".join(board)

for z in range(0,4):
	for y in range(0,4):
		for x in range(0,4):
			print "put("
			print draw(x,y,z,"_") + ",\n"
			print "  [%d, %d, %d], P," % (z,y,x) + "\n"
			print draw(x,y,z,"P")
			print ") :- !."
			print ""

def draw_empty(x, y, z):
	board=[]
	for k in range(0,4):
	    plane=[]
	    for j in range(0,4):
	        line=[]
	        for i in range(0,4):
	            if (z == 3-k and y == j and x == i):
	                line.append("X")
	            else:
	                line.append("_")
	        plane.append("  " + " / ".join(line))
	    board.append(" /\n".join(plane))
	return " /\n\n".join(board)

for z in range(0,4):
	for y in range(0,4):
		for x in range(0,4):
			print 'is_empty('
			print draw_empty(x,y,z) + ","
			print '  [%d, %d, %d]) :- ' % (z,y,x)
			print '  X = 0, !.'
			print ''

