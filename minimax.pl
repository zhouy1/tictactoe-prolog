:- module(minimax,
        [ minimax/6            % The minimax algorithm
        ]).

:- use_module(board,
        [ moves/3,
          opponent/2,
          empty_board/1,
          print_board/1,
          me/1
        ]).

:- use_module(heuristics,
        [ h_func/4
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Minimax
%%%
%%%
%%%     This is a minimax algorithm using alpha-beta pruning
%%%
%%%         minimax(+Board, -[Z,Y,X], -NextBoard, +Depth)
%%%
%%%         where Board is the current board state
%%%               BestMove is the best selected move
%%%               NextBoard is the next board state (after move)
%%%               Depth is the analysis depth (must be > 0)
%%%
%%%         if there is no more moves available, it fails

minimax(Board, ToMove, BestMove, NextBoard, Val, Depth) :-
  Depth > 0,
  nb_setval(branches, 0),
  alphabeta(node(_,Board), ToMove, 0/1000000, node(BestMove,NextBoard), Val, Depth), !.

alphabeta(node(Pos,Board), ToMove, _, _, Val, 0) :-
  opponent(ToMove, Opponent),
  heuristics:h_func(Board, Pos, ToMove, MyVal),
  heuristics:h_func(Board, Pos, Opponent, OtherVal),
  Val is MyVal-OtherVal, !.

alphabeta(Board, ToMove, Bounds, GoodBoard, Val, Depth) :-
  nb_getval(branches, N), NC is N+1, nb_setval(branches, NC),
  board:moves(Board, ToMove, NextBoards),
  OneDeeper is Depth - 1,
  boundedbest(NextBoards, ToMove, Bounds, GoodBoard, Val, OneDeeper), !.

boundedbest([Board|TailBoards], ToMove, Bounds, GoodBoard, GoodVal, Depth) :-
  opponent(ToMove, Opponent),
  alphabeta(Board, Opponent, Bounds, _, Val, Depth),
  goodenough(TailBoards, ToMove, Bounds, Board, Val, GoodBoard, GoodVal, Depth), !.

goodenough([], _, _, Board, Val, Board, Val, _) :- !.    % No other candidate

goodenough(_, ToMove, Alpha/Beta, Board, Val, Board, Val, _) :-
  min_to_move(ToMove), Val > Beta, !                 % Maximizer attained upper bound
  ;
  max_to_move(ToMove), Val < Alpha, !.               % Minimizer attained lower bound

goodenough(TailBoards, ToMove, Bounds, Board, Val, GoodBoard, GoodVal, Depth)  :-
  newbounds(Bounds, ToMove, Val, NewBounds),         % Refine bounds
  boundedbest(TailBoards, ToMove, NewBounds, Board1, Val1, Depth),
  betterof(Board, ToMove, Val, Board1, Val1, GoodBoard, GoodVal), !.

newbounds(Alpha/Beta, ToMove, Val, Val/Beta) :-
  min_to_move(ToMove), Val > Alpha, !.               % Maximizer increased lower bound

newbounds(Alpha/Beta, ToMove, Val, Alpha/Val) :-
  max_to_move(ToMove), Val < Beta, !.                % Minimizer decreased upper bound

newbounds(Alpha/Beta, _, _, Alpha/Beta) :- !.        % Otherwise bounds unchanged

betterof(Board1, ToMove, Val1, _, Val2, Board1, Val1) :-      % Board1 better than Board2
  min_to_move(ToMove), Val1 > Val2, !
  ;
  max_to_move(ToMove), Val1 < Val2, !.

betterof(_, _, _, Board2, Val2, Board2, Val2) :- !.  % Otherwise Board2 is better

max_to_move(X) :- me(X), !.
min_to_move(X) :- \+ max_to_move(X), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(minimax).
:- use_module(moves, [ put/4 ]).

test(first_move) :-
  board:empty_board(Em),
  minimax(Em, x, [Z,Y,X], _, Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl.

test(next_move) :-
  Board =
    o / 0 / 0 / 0 /
    o / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    x / x / 0 / 0 /
    0 / 0 / 0 / 0 /
    x / x / 0 / 0 /
    0 / 0 / 0 / 0 /


    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0,

  minimax(Board, x, [Z,Y,X], _, Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl.

test(win_next) :-
  Board =
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    o / 0 / 0 / 0 /
    o / 0 / 0 / 0 /
    o / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    x / 0 / 0 / 0 /
    x / 0 / 0 / 0 /
    x / 0 / 0 / 0 /
    0 / 0 / 0 / 0,

  minimax(Board, x, [Z,Y,X], _, Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl,
  [Z,Y,X] = [0,3,0].

:- end_tests(minimax).

