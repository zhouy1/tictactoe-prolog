print ':- module(heuristics,'
print '        [ first_move/2,          % The heuristic for first move'
print '          value/3                % The heuristic for board state'
print '        ]).'
print ''
print ':- use_module(board,'
print '        [ me/1,'
print '          opponent/2,'
print '          empty_board/1,'
print '          moves/2'
print '        ]).'
print ''
print '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
print '%%%'
print '%%%     Heuristics'
print '%%%'
print '%%%'
print '%%%     The h_func calculates the heuristics function for a'
print '%%%     given position of the board.'
print '%%%'
print '%%%         h_func(+Board, +Player, -Weight)'
print '%%%'
print '%%%         where Board is the current board state'
print '%%%               Player is the piece of current player (x/o)'
print '%%%               Weight is the heuristic value'
print ''
print 'not_opponent(A,B,C,D,x) :-'
print '  (A = x; A = 0),'
print '  (B = x; B = 0),'
print '  (C = x; C = 0),'
print '  (D = x; D = 0).'
print ''
print 'not_opponent(A,B,C,D,o) :-'
print '  (A = o; A = 0),'
print '  (B = o; B = 0),'
print '  (C = o; C = 0),'
print '  (D = o; D = 0).'
print ''
print 'found(0,0) :- !.'
print 'found(_,1) :- !.'
print ''
print 'w(0,0) :- !.      % no pieces, then 0'
print 'w(1,100) :- !.    % one piece, then 100'
print 'w(2,1000) :- !.   % two pieces, then 1000'
print 'w(3,10000) :- !.  % three pieces, then 10000'
print 'w(4,100000) :- !. % four pieces, then 100000'
print ''
print 'weight(A,B,C,D,Player,W) :-'
print '  ('
print '    not_opponent(A,B,C,D,Player) ->'
print '    ('
print '      found(A,W1),'
print '      found(B,W2),'
print '      found(C,W3),'
print '      found(D,W4),'
print '      S is W1+W2+W3+W4,'
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
		N=['W','Z','Y','X']
		pieces=[]
		for k in range(0,4):
			plane=[]
			for j in range(0,4):
				line=[]
				for i in range(0,4):
					line.append('%s%d%d' % (N[k],j,i))
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
	m[(z,y,x)]='_X'
	for board in boards:
		j=0
		for piece in board:
			if piece not in set(m.keys()):
				m[piece]='%s%d' % (i,j)
				j+=1
		i=chr(ord(i) + 1)
	return m

def single_merge(board):
	i='A'
	m=dict()
	for piece in board:
		m[piece]='%s' % i
		i=chr(ord(i) + 1)
	return m

def print_h(board, i):
	line=[]
	for (z,y,x) in board.keys():
		N=['W','Z','Y','X']
		line.append('%s%d%d' % (N[z],y,x))

	print '  weight(' + ','.join(line) + ',Player,S%d),' % i

def print_crosses((z,y,x), weight):
	print 'crosses([%d, %d, %d], %d).' % (z,y,x,weight)

def heuristics():
	w=wins()

	print 'h_func('
	print str(Board()) + ','
	print '  Player, W) :- '
	print ''

	i = 1
	for board in w:
		M=single_merge(board)
		print_h(M, i)
		i += 1

	l=[]
	for j in range(0,i-1):
		l.append('S%d' % (j+1))

	print '  W is ' + '+'.join(l) + ', !.'
	print ''

	for z in range(0,4):
		for y in range(0,4):
			for x in range(0,4):
				I=intercepts(w, (z,y,x))
				M=merge((z,y,x),I)
				print_crosses((z,y,x),len(I))
	print ''

heuristics()

print 'first_move(BestMove, BestValue) :-'
print '  board:empty_board(E),'
print '  board:moves(E, Moves),'
print '  first_move(Moves, nil/0, BestMove/BestValue), !.'
print ''
print 'first_move([], Move/Val, Move/Val) :- !.'
print ''
print 'first_move([Move|TailMoves], SelMove/Val, BestMove/BestValue) :-'
print '  random(1,1000, R),'
print '  ((crosses(Move, X), X = 7) ->'
print '    (Score is 1000 + R) ; (Score is 100 + R)),'
print '  first_move(Move/Score, SelMove/Val, SelMove1/Val1),'
print '  first_move(TailMoves, SelMove1/Val1, BestMove/BestValue), !.'
print ''
print 'first_move(_/Val1, Move2/Val2, Move2/Val2) :-'
print '  Val2 > Val1, !.'
print ''
print 'first_move(Move1/Val1, _, Move1/Val1) :- !.'
print ''
print 'value(Board, Player, Val) :-'
print '  board:me(Me),'
print '  board:opponent(Me, Opponent),'
print '  h_func(Board, Me, W1),'
print '  h_func(Board, Opponent, W2),'
print '  scoreof(W1, W2, Player, Val), !.'
print ''
print 'scoreof(W1, W2, Player, Val) :-'
print '  board:me(Player), Val is W1-W2, !.'
print ''
print 'scoreof(W1, W2, _, Val) :-'
print '  Val is W2-W1, !.'
print ''

