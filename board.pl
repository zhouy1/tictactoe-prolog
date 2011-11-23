:- module(board,
        [ empty_board/1,         % Gives an empty 4x4x4 board
          print_board/1,         % Prints a given board
          opponent/1,            % Get the opponent player
          moves/2,               % Possible moves for a given board
          me/1,                  % Get my player
          put/4,                 % Put a piece on the board
          is_empty/2             % Check if a position is empty
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

me(x).
opponent(o).

empty_board(
  0/0/0/0 / 0/0/0/0 / 0/0/0/0 / 0/0/0/0 /
  0/0/0/0 / 0/0/0/0 / 0/0/0/0 / 0/0/0/0 /
  0/0/0/0 / 0/0/0/0 / 0/0/0/0 / 0/0/0/0 /
  0/0/0/0 / 0/0/0/0 / 0/0/0/0 / 0/0/0/0
).

stack(
  W00/W01/W02/W03 / W10/W11/W12/W13 / W20/W21/W22/W23 / W30/W31/W32/W33 /
  Z00/Z01/Z02/Z03 / Z10/Z11/Z12/Z13 / Z20/Z21/Z22/Z23 / Z30/Z31/Z32/Z33 /
  Y00/Y01/Y02/Y03 / Y10/Y11/Y12/Y13 / Y20/Y21/Y22/Y23 / Y30/Y31/Y32/Y33 /
  X00/X01/X02/X03 / X10/X11/X12/X13 / X20/X21/X22/X23 / X30/X31/X32/X33,

  [ X00,X01,X02,X03 , X10,X11,X12,X13 , X20,X21,X22,X23 , X30,X31,X32,X33 ,
    Y00,Y01,Y02,Y03 , Y10,Y11,Y12,Y13 , Y20,Y21,Y22,Y23 , Y30,Y31,Y32,Y33 ,
    Z00,Z01,Z02,Z03 , Z10,Z11,Z12,Z13 , Z20,Z21,Z22,Z23 , Z30,Z31,Z32,Z33 ,
    W00,W01,W02,W03 , W10,W11,W12,W13 , W20,W21,W22,W23 , W30,W31,W32,W33 ]
).

chr(0, '_').
chr(x, 'x').
chr(o, 'o').

print_cells([H]) :-
  chr(H, Chr),
  write(Chr),nl.

print_cells([H|T]) :-
  chr(H, Chr),
  write(Chr),write('/'),
  print_cells(T).

print_lines([]).

print_lines([Line|Tail]) :-
  print_cells(Line),
  print_lines(Tail).

print_planes([Plane]) :-
  group(Plane, 4, Lines),
  print_lines(Lines).

print_planes([Plane|Tail]) :-
  group(Plane, 4, Lines),
  print_lines(Lines),
  write('-------'),nl,
  print_planes(Tail).

print_board(Board) :-
  stack(Board, Stack),
  group(Stack, 16, Planes),
  print_planes(Planes),
  nl.

group([],0,_,Sub,Aux,[Sub|Aux]).

group(L,0,K,Sub,Aux,R) :-
  group(L,K,K,[],[Sub|Aux],R).

group([H|T],N,K,Sub,Aux,R) :-
  N > 0,
  N1 is N-1,
  group(T,N1,K,[H|Sub],Aux,R).

group(L,K,R) :-
  group(L,K,K,[],[],R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Next moves
%%%
%%%
%%%     Next moves have the following form
%%%
%%%         node(Pos,Board)
%%%
%%%         where Pos is the next possible move
%%%               Board is the board state after this move

moves(Board, Moves) :-
  findall(Pos,
    (bp(Pos),
      is_empty(Board, Pos)),
    Moves).

% random order of board moves, prioritized by the best
% moves, i.e. those moves having 7 crossing wins

bp([2,1,1]). bp([1,2,2]). bp([1,2,1]). bp([1,1,1]).
bp([3,0,0]). bp([1,1,2]). bp([2,2,2]). bp([3,3,0]).
bp([2,2,1]). bp([0,3,3]). bp([0,0,0]). bp([3,3,3]).
bp([2,1,2]). bp([0,3,0]). bp([0,0,3]). bp([3,0,3]).
bp([3,1,2]). bp([1,1,3]). bp([3,2,0]). bp([0,2,0]).
bp([2,0,3]). bp([2,3,2]). bp([1,2,3]). bp([3,0,1]).
bp([2,0,0]). bp([1,0,2]). bp([1,3,2]). bp([1,0,0]).
bp([3,3,1]). bp([3,1,0]). bp([2,2,3]). bp([3,2,3]).
bp([1,1,0]). bp([1,2,0]). bp([1,0,3]). bp([0,2,2]).
bp([1,3,1]). bp([0,3,1]). bp([2,1,0]). bp([0,1,0]).
bp([0,2,1]). bp([2,3,3]). bp([1,3,3]). bp([0,3,2]).
bp([1,3,0]). bp([0,1,1]). bp([2,2,0]). bp([3,1,1]).
bp([2,3,1]). bp([3,0,2]). bp([2,0,2]). bp([1,0,1]).
bp([2,3,0]). bp([0,1,3]). bp([3,2,2]). bp([0,0,2]).
bp([0,0,1]). bp([0,2,3]). bp([2,1,3]). bp([3,3,2]).
bp([3,2,1]). bp([3,1,3]). bp([0,1,2]). bp([2,0,1]).

stackpos([Z,Y,X], N) :-
  N is Z * 16 + Y * 4 + X.

put(Board, Pos, P, NewBoard) :-
  stack(Board, Stack),
  stackpos(Pos, R),
  insert_at(P, Stack, R, NewStack),
  stack(NewBoard, NewStack).

insert_at(X,[_|Xs],0,[X|Xs]).

insert_at(X,[Y|Xs],K,[Y|Ys]) :-
  K > 0, 
  K1 is K - 1,
  insert_at(X,Xs,K1,Ys).

is_empty(Board, Pos) :-
  stack(Board, Stack),
  stackpos(Pos, R),
  element_at(X, Stack, R), X = 0.

element_at(X,[X|_],0).

element_at(X,[_|L],K) :-
  K > 0,
  K1 is K - 1,
  element_at(X,L,K1).

