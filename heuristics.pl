:- module(heuristics,
        [ first_move/2,          % The heuristic for first move
          value/3,               % The heuristic for board state
          win/2                  % Whether some player wins
        ]).

:- use_module(board,
        [ me/1,
          opponent/2,
          empty_board/1,
          moves/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Heuristics
%%%
%%%
%%%     The h_func calculates the heuristics function for a
%%%     given position of the board.
%%%
%%%         h_func(+Board, +Player, -Weight)
%%%
%%%         where Board is the current board state
%%%               Player is the piece of current player (x/o)
%%%               Weight is the heuristic value

weight(0, 0).      % no pieces, then 0
weight(1, 100).    % one piece, then 100
weight(2, 1000).   % two pieces, then 1000
weight(3, 10000).  % three pieces, then 10000
weight(4, 900000). % four pieces, then 900000

score(A, B, C, D, Player, W) :-
  in_row([A, B, C, D], Player, 0, Sum),
  weight(Sum, W)
  ;
  W = 0.

in_row([], _, Sum, Sum).

in_row([H|T], Player, Aux, Sum) :-
  H = Player -> Aux1 is Aux+1 ;
  H = 0 -> Aux1 is Aux,
  find(T, Player, Aux1, Sum).

wins(
  X00/X01/X02/X03 / X10/X11/X12/X13 / X20/X21/X22/X23 / X30/X31/X32/X33 /
  Y00/Y01/Y02/Y03 / Y10/Y11/Y12/Y13 / Y20/Y21/Y22/Y23 / Y30/Y31/Y32/Y33 /
  Z00/Z01/Z02/Z03 / Z10/Z11/Z12/Z13 / Z20/Z21/Z22/Z23 / Z30/Z31/Z32/Z33 /
  W00/W01/W02/W03 / W10/W11/W12/W13 / W20/W21/W22/W23 / W30/W31/W32/W33,

  [ % all wins for a 4x4x4x4 board
    (Z22,W33,X00,Y11), (W30,Z21,Y12,X03), (X33,Y22,Z11,W00), (Y21,X30,W03,Z12),
    (W30,X00,Y10,Z20), (X30,Z10,W00,Y20), (X00,Z02,Y01,W03), (Y02,Z01,W00,X03),
    (W30,W12,W21,W03), (W11,W33,W00,W22), (Z00,X00,W00,Y00), (W30,W20,W10,W00),
    (W00,W01,W02,W03), (Z10,W10,X10,Y10), (Z00,Z10,Z20,Z30), (Z00,Z03,Z02,Z01),
    (X20,W20,Z20,Y20), (Y10,Y00,Y30,Y20), (Y02,Y03,Y00,Y01), (W30,X30,Z30,Y30),
    (X20,X30,X00,X10), (X02,X01,X00,X03), (X01,Y11,W31,Z21), (Y21,X31,W01,Z11),
    (Z12,X10,W13,Y11), (Z11,W10,Y12,X13), (Z03,Z21,Z30,Z12), (Z22,Z00,Z11,Z33),
    (X01,W01,Y01,Z01), (W11,W21,W01,W31), (W12,W11,W10,W13), (W11,Z11,Y11,X11),
    (Z21,Z11,Z31,Z01), (Z13,Z10,Z11,Z12), (Y21,Z21,W21,X21), (Y21,Y11,Y31,Y01),
    (Y11,Y10,Y13,Y12), (X31,Z31,Y31,W31), (X01,X31,X11,X21), (X10,X11,X12,X13),
    (X02,Z22,W32,Y12), (Y22,Z12,W02,X32), (X20,Y21,W23,Z22), (X23,W20,Z21,Y22),
    (Y21,Y03,Y12,Y30), (Y22,Y33,Y00,Y11), (X02,Z02,Y02,W02), (W12,W32,W02,W22),
    (W23,W20,W21,W22), (W12,X12,Y12,Z12), (Z22,Z02,Z32,Z12), (Z22,Z21,Z20,Z23),
    (Z22,Y22,X22,W22), (Y22,Y02,Y32,Y12), (Y21,Y22,Y23,Y20), (W32,Y32,Z32,X32),
    (X02,X22,X12,X32), (X20,X23,X22,X21), (W33,Z23,Y13,X03), (X33,Z13,Y23,W03),
    (X30,W33,Y31,Z32), (X33,Z31,Y32,W30), (X30,X03,X12,X21), (X33,X22,X11,X00),
    (Z03,Y03,W03,X03), (W23,W33,W03,W13), (W30,W33,W32,W31), (Z13,Y13,X13,W13),
    (Z13,Z03,Z33,Z23), (Z30,Z31,Z32,Z33), (W23,X23,Y23,Z23), (Y23,Y33,Y03,Y13),
    (Y33,Y32,Y31,Y30), (X33,W33,Y33,Z33), (X33,X23,X13,X03), (X33,X30,X31,X32)
  ]).

h_func(Board, Player, W) :-
  wins(Board, Stack),
  h_sum(Stack, Player, 0, W).

h_sum([], _, W, W).

h_sum([(A,B,C,D)|Tail], Player, S, W) :-
  score(A, B, C, D, Player, Score),
  S1 is S + Score,
  h_sum(Tail, Player, S1, W).

crosses([Z,Y,X],C) :-
  (
    [Z,Y,X] = [0,0,0]; [Z,Y,X] = [0,0,3]; [Z,Y,X] = [0,3,0]; [Z,Y,X] = [0,3,3];
    [Z,Y,X] = [1,1,1]; [Z,Y,X] = [1,1,2]; [Z,Y,X] = [1,2,1]; [Z,Y,X] = [1,2,2];
    [Z,Y,X] = [2,1,1]; [Z,Y,X] = [2,1,2]; [Z,Y,X] = [2,2,1]; [Z,Y,X] = [2,2,2];
    [Z,Y,X] = [3,0,0]; [Z,Y,X] = [3,0,3]; [Z,Y,X] = [3,3,0]; [Z,Y,X] = [3,3,3]
  )
  -> C = 7 ; C = 4.

win(Board, Player) :-
  wins(Board, Stack),
  find_win(Stack, Player) ; fail.

find_win([(A,B,C,D)|Tail], Player) :-
  in_row([A, B, C, D], Player, 0, Sum),
  Sum = 4 -> true ; find_win(Tail, Player).

first_move(BestMove, BestValue) :-
  board:empty_board(E),
  board:moves(E, Moves),
  first_move(Moves, nil/0, BestMove/BestValue), !.

first_move([], Move/Val, Move/Val) :- !.

first_move([Move|TailMoves], SelMove/Val, BestMove/BestValue) :-
  random(1,1000, R),
  ((crosses(Move, X), X = 7) ->
    (Score is 1000 + R) ; (Score is 100 + R)),
  first_move(Move/Score, SelMove/Val, SelMove1/Val1),
  first_move(TailMoves, SelMove1/Val1, BestMove/BestValue), !.

first_move(_/Val1, Move2/Val2, Move2/Val2) :-
  Val2 > Val1, !.

first_move(Move1/Val1, _, Move1/Val1).

value(Board, Player, Val) :-
  board:me(Me),
  board:opponent(Me, Opponent),
  h_func(Board, Me, W1),
  h_func(Board, Opponent, W2),
  scoreof(W1, W2, Player, Val).

scoreof(W1, W2, Player, Val) :-
  board:me(Player), Val is W1-W2, !.

scoreof(W1, W2, _, Val) :-
  Val is W2-W1.

