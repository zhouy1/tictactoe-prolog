:- module(board,
        [ emptyBoard/1,          % Gives an empty 4x4x4 board
          printBoard/1           % Prints a given board
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Board
%%%
%%%
%%%     The board have the form
%%%
%%%         W00/.../W33/Z00/.../Z33/Y00/.../Y33/X00/.../X33
%%%
%%%         X is the first plane, Y the second and so on
%%%
%%%         where 0 means empty
%%%               x means my move
%%%               o means opponent move

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

printCell(0) :- write(' ').
printCell(x) :- write('X').
printCell(o) :- write('O').

printLine(A, B, C, D) :-
  printCell(A),write('/'),
  printCell(B),write('/'),
  printCell(C),write('/'),
  printCell(D),nl.

printPlane(
  E00 / E01 / E02 / E03 /
  E10 / E11 / E12 / E13 /
  E20 / E21 / E22 / E23 /
  E30 / E31 / E32 / E33
) :-
  printLine(E00,E01,E02,E03),
  printLine(E10,E11,E12,E13),
  printLine(E20,E21,E22,E23),
  printLine(E30,E31,E32,E33).

printBoard(
  W00 / W01 / W02 / W03 /
  W10 / W11 / W12 / W13 /
  W20 / W21 / W22 / W23 /
  W30 / W31 / W32 / W33 /
                        
  Z00 / Z01 / Z02 / Z03 /
  Z10 / Z11 / Z12 / Z13 /
  Z20 / Z21 / Z22 / Z23 /
  Z30 / Z31 / Z32 / Z33 /
                        
  Y00 / Y01 / Y02 / Y03 /
  Y10 / Y11 / Y12 / Y13 /
  Y20 / Y21 / Y22 / Y23 /
  Y30 / Y31 / Y32 / Y33 /
                        
  X00 / X01 / X02 / X03 /
  X10 / X11 / X12 / X13 /
  X20 / X21 / X22 / X23 /
  X30 / X31 / X32 / X33
) :-
  printPlane(
    W00 / W01 / W02 / W03 /
    W10 / W11 / W12 / W13 /
    W20 / W21 / W22 / W23 /
    W30 / W31 / W32 / W33
  ),
  write('-------'),nl,
  printPlane(
    Z00 / Z01 / Z02 / Z03 /
    Z10 / Z11 / Z12 / Z13 /
    Z20 / Z21 / Z22 / Z23 /
    Z30 / Z31 / Z32 / Z33
  ),
  write('-------'),nl,
  printPlane(
    Y00 / Y01 / Y02 / Y03 /
    Y10 / Y11 / Y12 / Y13 /
    Y20 / Y21 / Y22 / Y23 /
    Y30 / Y31 / Y32 / Y33
  ),
  write('-------'),nl,
  printPlane(
    X00 / X01 / X02 / X03 /
    X10 / X11 / X12 / X13 /
    X20 / X21 / X22 / X23 /
    X30 / X31 / X32 / X33
  ),
  nl.

