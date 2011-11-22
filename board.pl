:- module(board,
        [ empty_board/1,         % Gives an empty 4x4x4 board
          print_board/1,         % Prints a given board
          opponent/2,            % Get the opponent player
          moves/2,               % Possible moves for a given board
          me/1,                  % Get my player
          put/4,                 % Put a piece on the board
          is_empty/2             % Check if a position is empty
        ]).

:- use_module(moves,
        [ put/4,
          is_empty/2
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

empty_board(
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

opponent(x,o) :- !.
opponent(o,x) :- !.

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
%%%         node(Pos,Board)
%%%
%%%         where Pos is the next possible move
%%%               Board is the board state after this move

moves(Board, Moves) :-
  findall(Pos,
    (bp(Pos),
      moves:is_empty(Board, Pos)),
    Moves).

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

