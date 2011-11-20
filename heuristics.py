print ':- module(heuristics,'
print '        [ h_func/4               % The heuristic function for each'
print '        ]).                      % position of the board.'
print ''
print '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
print '%%%'
print '%%%     Heuristics'
print '%%%'
print '%%%'
print '%%%     The h_func calculates the heuristics function for a'
print '%%%     given position of the board.'
print '%%%'
print '%%%         h_func(+Board, +[Z,Y,X], +Piece, -Weight)'
print '%%%'
print '%%%         where Board is the current board state'
print '%%%               [Z,Y,X] is the board position'
print '%%%               Piece is the piece of current player (x/o)'
print '%%%               Weight is the heuristic value'
print ''
print 'not_opponent(A,B,C,x) :-'
print '  (A = x; A = 0),'
print '  (B = x; B = 0),'
print '  (C = x; C = 0).'
print ''
print 'not_opponent(A,B,C,o) :-'
print '  (A = o; A = 0),'
print '  (B = o; B = 0),'
print '  (C = o; C = 0).'
print ''
print 'found(0,0) :- !.'
print 'found(_,1) :- !.'
print ''
print 'w(0,0) :- !.      % no pieces, then 0'
print 'w(1,10) :- !.     % one piece, then 10'
print 'w(2,100) :- !.    % two pieces, then 100'
print 'w(3,1000) :- !.   % three pieces, then 1000'
print ''
print 'weight(A,B,C,Piece,W) :-'
print '  ('
print '    not_opponent(A,B,C,Piece) ->'
print '    ('
print '      found(A,W1),'
print '      found(B,W2),'
print '      found(C,W3),'
print '      S is W1+W2+W3,'
print '      w(S,W)'
print '    ) ;'
print '    W = 0'
print '  ), !.'
print ''

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
	print '  [%d, %d, %d], Piece, W) :- ' % (z,y,x)
	print ''
	print '  (X = 0 ->'
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

	print '    % W(Board,Position) = Intercepts(Position) + Sum(H(Board,Position))'
	print '    W is %d+' % weight + '+'.join(t)
	print '  )'
	print '  ; W = 0), !.'
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

