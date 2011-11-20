:- module(board,
        [ empty/1,               % Gives an empty 4x4x4 board
          print_board/1,         % Prints a given board
          other_player/2,        % Get the other player
          moves/3                % Possible moves for a given board
        ]).

:- use_module(heuristics,
        [ h_func/4
        ]).

:- use_module(moves,
        [ put/4
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

empty(
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
) :- !.

other_player(x,o) :- !.
other_player(o,x) :- !.

print_cell(0) :- write(' '), !.
print_cell(x) :- write('X'), !.
print_cell(o) :- write('O'), !.

print_line(A, B, C, D) :-
  print_cell(A),write('/'),
  print_cell(B),write('/'),
  print_cell(C),write('/'),
  print_cell(D),nl,
  !.

print_plane(
  E00 / E01 / E02 / E03 /
  E10 / E11 / E12 / E13 /
  E20 / E21 / E22 / E23 /
  E30 / E31 / E32 / E33
) :-
  print_line(E00,E01,E02,E03),
  print_line(E10,E11,E12,E13),
  print_line(E20,E21,E22,E23),
  print_line(E30,E31,E32,E33),
  !.

print_board(
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
  print_plane(
    W00 / W01 / W02 / W03 /
    W10 / W11 / W12 / W13 /
    W20 / W21 / W22 / W23 /
    W30 / W31 / W32 / W33
  ),
  write('-------'),nl,
  print_plane(
    Z00 / Z01 / Z02 / Z03 /
    Z10 / Z11 / Z12 / Z13 /
    Z20 / Z21 / Z22 / Z23 /
    Z30 / Z31 / Z32 / Z33
  ),
  write('-------'),nl,
  print_plane(
    Y00 / Y01 / Y02 / Y03 /
    Y10 / Y11 / Y12 / Y13 /
    Y20 / Y21 / Y22 / Y23 /
    Y30 / Y31 / Y32 / Y33
  ),
  write('-------'),nl,
  print_plane(
    X00 / X01 / X02 / X03 /
    X10 / X11 / X12 / X13 /
    X20 / X21 / X22 / X23 /
    X30 / X31 / X32 / X33
  ),
  nl,
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Next moves
%%%
%%%
%%%     Next moves have the following form
%%%
%%%         node(Pos,Board,Weight)
%%%
%%%         where Pos is the previous move
%%%               Board is the board state after this move
%%%               Weight is the board evaluation

moves(Board, Piece, NextBoards) :-
  moves(Board, Piece, [0,0,0], [], NextBoards), !.

moves(board([_,_,_],Board,_), Piece, NextBoards) :-
  moves(Board, Piece, [0,0,0], [], NextBoards), !.

moves(_, _, [4,0,0], BoardStack, BoardStack) :- !.

moves(Board, Piece, CurPos, BoardStack, NextBoards) :-
  heuristics:h_func(Board, CurPos, Piece, Weight),
  insert(CurPos, Piece, Board, Weight, BoardStack, NewBoardStack),
  iterate(CurPos, NextPos),
  moves(Board, Piece, NextPos, NewBoardStack, NextBoards).

insert(_, _, _, Weight, Tail, Tail) :-
  Weight =< 0, !.

insert(Pos, Piece, Board, Weight, Tail, [node(Pos,NewBoard,Weight)|Tail]) :-
  put(Board, Pos, Piece, NewBoard), !.

iterate([Z,Y,X], [Z1,Y1,X1]) :-
  (X1 is (X+1) mod 4),
  (X1 = 0 -> Y1 is (Y+1) mod 4 ; Y1 = Y),
  ((X1 = 0, Y1 = 0) -> Z1 is Z+1 ; Z1 = Z), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(board).
:- use_module(board, [ empty/1 ]).

test(should_insert) :-
  empty(Em),
  moves:put(Em, [0,0,0], x, B1),
  moves:put(B1, [0,1,0], x, B2),
  moves:put(B2, [0,2,0], x, B3),
  moves:put(B3, [1,0,0], o, B4),
  moves:put(B4, [1,1,0], o, B5),
  moves:put(B5, [1,2,0], o, Board),
  insert([0,3,0], x, Board, 1, [], Tail),
  Tail \= [].

test(should_find_children) :-
  empty(Em),
  moves:put(Em, [0,0,0], x, B1),
  moves:put(B1, [0,1,0], x, B2),
  moves:put(B2, [0,2,0], x, B3),
  moves:put(B3, [1,0,0], o, B4),
  moves:put(B4, [1,1,0], o, B5),
  moves:put(B5, [1,2,0], o, Board),
  moves(Board, x, NextBoards),
  NextBoards \= [].

:- end_tests(board).

