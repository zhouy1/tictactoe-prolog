class Board:
	def __init__(self, pieces = []):
		self.pieces=pieces

	def __str__(self):
		return " /\n\n".join([" /\n".join(["  " + "/".join(line) for line in plane]) for plane in self.board()])

	def board(self):
		pieces=[]
		for k in range(0,4):
			plane=[]
			for j in range(0,4):
				line=[]
				for i in range(0,4):
					if (k,j,i) in self.pieces.keys():
						line.append(self.pieces[(k,j,i)])
					else:
						line.append("_ ")
				plane.append(line)
			pieces.append(plane)
		pieces.reverse()
		return pieces

def wins():
	w=[]
	w.append([(3,0,0), \
			  (2,1,1), \
			  (1,2,2), \
			  (0,3,3)])
	w.append([(3,0,3), \
			  (2,1,2), \
			  (1,2,1), \
			  (0,3,0)])
	w.append([(3,3,3), \
			  (2,2,2), \
			  (1,1,1), \
			  (0,0,0)])
	w.append([(3,3,0), \
			  (2,2,1), \
			  (1,1,2), \
			  (0,0,3)])
	for j in range(0,4):
		w.append([(3,0,j), \
				  (2,1,j), \
				  (1,2,j), \
				  (0,3,j)])
		w.append([(3,3,j), \
				  (2,2,j), \
				  (1,1,j), \
				  (0,0,j)])
		w.append([(3,j,0), \
				  (2,j,1), \
				  (1,j,2), \
				  (0,j,3)])
		w.append([(3,j,3), \
				  (2,j,2), \
				  (1,j,1), \
				  (0,j,0)])
		w.append([(j,3,0), \
				  (j,2,1), \
				  (j,1,2), \
				  (j,0,3)])
		w.append([(j,3,3), \
				  (j,2,2), \
				  (j,1,1), \
				  (j,0,0)])
		for i in range(0,4):
			w.append([(0,i,j), \
					  (1,i,j), \
					  (2,i,j), \
					  (3,i,j)])
			w.append([(i,0,j), \
					  (i,1,j), \
					  (i,2,j), \
					  (i,3,j)])
			w.append([(i,j,0), \
					  (i,j,1), \
					  (i,j,2), \
					  (i,j,3)])
	return w

def intercepts(wins, piece):
	r=[]
	for board in wins:
		if piece in set(board):
			r.append(board)
	return r

def merge((z,y,x), boards):
	i='A'
	m=dict()
	m[(z,y,x)]='X '
	for board in boards:
		j=0
		for piece in board:
			if piece not in set(m.keys()):
				m[piece]='%s%d' % (i,j)
				j+=1
		i=chr(ord(i) + 1)
	return m

def print_h((z,y,x), intercepts, board, weight):
	print 'h_func('
	print str(Board(board)) + ','
	print '  %d, %d, %d, Piece, W) :- ' % (z,y,x)
	print '  X = 0 ->'
	print '  ('

	i=0
	t=[]
	v=[]
	for piece in sorted(board.values()):
		if piece == 'X ': break
		v.append(piece)
		i+=1
		if (i % 3) == 0:
			var='W%d' % (i/3)
			t.append(var)
			print '    weight(' + ','.join(v) + ',Piece,%s),' % var
			v=[]

	print '    W is %d+' % weight + '+'.join(t)
	print '  )'
	print '  ; W = 0.'
	print ''

def heuristics():
	w=wins()

	for z in range(0,4):
		for y in range(0,4):
			for x in range(0,4):
				I=intercepts(w, (z,y,x))
				M=merge((z,y,x),I)
				print_h((z,y,x),I,M,len(I))

heuristics()

