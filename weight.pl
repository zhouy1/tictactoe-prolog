not_oponent(A,B,C,Piece) :-
  Piece = x ->
  (
    (A = x; A = 0),
    (B = x; B = 0),
    (C = x; C = 0)
  ) ;
  Piece = o ->
  (
    (A = o; A = 0),
    (B = o; B = 0),
    (C = o; C = 0)
  ).

found(0,0).
found(_,1).

w(0,0).
w(1,10).
w(2,100).
w(3,1000).

weight(A,B,C,Piece,W) :-
  not_oponent(A,B,C,Piece) ->
  (
    found(A,W1),
    found(B,W2),
    found(C,W3),
    S is W1+W2+W3,
    w(S,W)
  ) ;
  (
    W = 0
  ).

