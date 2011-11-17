class Board:
    def __init__(self, pieces = []):
        self.pieces = pieces

    def draw(self):
        board=[]
        bs=set(self.pieces)
        for k in range(0,4):
            plane=[]
            for j in range(0,4):
                line=[]
                for i in range(0,4):
                    if ((k,j,i) in bs):
                        line.append("X")
                    else:
                        line.append("_")
                plane.append("  " + "/".join(line))
            board.append(" /\n".join(plane))
        return " /\n\n".join(board)

def draw(board):
	print 'wins('
	print Board(board).draw() + ','
	print '  X).'
	print ''

def wins():
	draw([(3,0,0),(2,1,1),(1,2,2),(0,3,3)])
	draw([(0,0,0), \
          (1,1,1), \
          (2,2,2), \
          (3,3,3)])
	for j in range(0,4):
		draw([(3,0,j), \
              (2,1,j), \
              (1,2,j), \
              (0,3,j)])
		draw([(0,3,j), \
              (1,2,j), \
              (2,1,j), \
              (3,0,j)])
		draw([(3,j,0),  
              (2,j,1), \
              (1,j,2), \
              (0,j,3)])
		draw([(3,j,3), \
              (2,j,2), \
              (1,j,1), \
              (0,j,0)])
		draw([(j,3,0), \
              (j,2,1), \
              (j,1,2), \
              (j,0,3)])
		draw([(j,3,3), \
              (j,2,2), \
              (j,1,1), \
              (j,0,0)])
		for i in range(0,4):
			draw([(0,i,j), \
                  (1,i,j), \
                  (2,i,j), \
                  (3,i,j)])
			draw([(i,0,j), \
                  (i,1,j), \
                  (i,2,j), \
                  (i,3,j)])
			draw([(i,j,0), \
                  (i,j,1), \
                  (i,j,2), \
                  (i,j,3)])

wins()

