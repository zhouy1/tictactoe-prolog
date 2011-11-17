emptyBoard(
  0/0/0/0/
  0/0/0/0/
  0/0/0/0/
  0/0/0/0/

  0/0/0/0/
  0/0/0/0/
  0/0/0/0/
  0/0/0/0/

  0/0/0/0/
  0/0/0/0/
  0/0/0/0/
  0/0/0/0/

  0/0/0/0/
  0/0/0/0/
  0/0/0/0/
  0/0/0/0
).

cell(0) :- write(' ').
cell(x) :- write('X').
cell(o) :- write('O').

printBoard(
  X11/X12/X13/X14/
  X21/X22/X23/X24/
  X31/X32/X33/X34/
  X41/X42/X43/X44/

  Y11/Y12/Y13/Y14/
  Y21/Y22/Y23/Y24/
  Y31/Y32/Y33/Y34/
  Y41/Y42/Y43/Y44/

  Z11/Z12/Z13/Z14/
  Z21/Z22/Z23/Z24/
  Z31/Z32/Z33/Z34/
  Z41/Z42/Z43/Z44/

  W11/W12/W13/W14/
  W21/W22/W23/W24/
  W31/W32/W33/W34/
  W41/W42/W43/W44
) :-
  cell(X11),write('/'),cell(X12),write('/'),cell(X13),write('/'),cell(X14),nl,
  cell(X21),write('/'),cell(X22),write('/'),cell(X23),write('/'),cell(X24),nl,
  cell(X31),write('/'),cell(X32),write('/'),cell(X33),write('/'),cell(X34),nl,
  cell(X41),write('/'),cell(X42),write('/'),cell(X43),write('/'),cell(X44),nl,
  write('-------'),nl,
  cell(Y11),write('/'),cell(Y12),write('/'),cell(Y13),write('/'),cell(Y14),nl,
  cell(Y21),write('/'),cell(Y22),write('/'),cell(Y23),write('/'),cell(Y24),nl,
  cell(Y31),write('/'),cell(Y32),write('/'),cell(Y33),write('/'),cell(Y34),nl,
  cell(Y41),write('/'),cell(Y42),write('/'),cell(Y43),write('/'),cell(Y44),nl,
  write('-------'),nl,
  cell(Z11),write('/'),cell(Z12),write('/'),cell(Z13),write('/'),cell(Z14),nl,
  cell(Z21),write('/'),cell(Z22),write('/'),cell(Z23),write('/'),cell(Z24),nl,
  cell(Z31),write('/'),cell(Z32),write('/'),cell(Z33),write('/'),cell(Z34),nl,
  cell(Z41),write('/'),cell(Z42),write('/'),cell(Z43),write('/'),cell(Z44),nl,
  write('-------'),nl,
  cell(W11),write('/'),cell(W12),write('/'),cell(W13),write('/'),cell(W14),nl,
  cell(W21),write('/'),cell(W22),write('/'),cell(W23),write('/'),cell(W24),nl,
  cell(W31),write('/'),cell(W32),write('/'),cell(W33),write('/'),cell(W34),nl,
  cell(W41),write('/'),cell(W42),write('/'),cell(W43),write('/'),cell(W44),nl,
  nl,
  nl.

