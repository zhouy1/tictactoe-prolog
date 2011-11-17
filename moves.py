def draw(x, y, z, p):
    N=['W','Z','Y','X']
    board=[]
    for k in range(0,4):
        plane=[]
        for j in range(0,4):
            line=[]
            for i in range(0,4):
                if (z == k and y == j and x == i):
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
            print "  %d, %d, %d, P," % (x,y,z) + "\n"
            print draw(x,y,z,"P")
            print ")."
            print ""

