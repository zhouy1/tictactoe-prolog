:- module(minimax,
        [ minimax/5            % The minimax algorithm
        ]).

:- use_module(board,
        [ moves/2,
          opponent/2,
          empty_board/1,
          print_board/1,
          me/1
        ]).

:- use_module(moves,
        [ put/4
        ]).

:- use_module(heuristics,
        [ h_func/3,
          crosses/2
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

minimax(Board, Player, BestMove, BestValue, Depth) :-
  Depth > 0,
  nb_setval(branches, 0),
  board:empty_board(Board) ->
    start_game(BestMove, BestValue), !
    ;
    alphabeta(nil/Board, Player, 0/10000000, BestMove, BestValue, Depth), !.

start_game(BestMove, BestValue) :-
  board:empty_board(E),
  board:moves(E, Moves),
  best_corner(Moves, nil/0, BestMove/BestValue).

best_corner([], Move/Val, Move/Val) :- !.

best_corner([Move|TailMoves], SelMove/Val, BestMove/BestValue) :-
  random(1,1000, R),
  ((crosses(Move, X), X = 7) ->
    (Score is 1000 + R) ; (Score is 100 + R)),
  best_corner(Move/Score, SelMove/Val, SelMove1/Val1),
  best_corner(TailMoves, SelMove1/Val1, BestMove/BestValue).

best_corner(_/Val1, Move2/Val2, Move2/Val2) :-
  Val2 > Val1, !.

best_corner(Move1/Val1, _, Move1/Val1).

sum_scores([], Val, Val) :- !.

sum_scores([Score|ScoreTail], Aux, Val) :-
  Aux1 is Aux + Score,
  sum_scores(ScoreTail, Aux1, Val), !.

alphabeta(Move/Board, Opponent, _, _, Val, 0) :-
  opponent(Opponent, Player),
  heuristics:h_func(Board, Player, W1),
  heuristics:h_func(Board, Opponent, W2),
  crosses(Move,H),
  Val is (H*H)+W1-W2,
%  board:print_board(Board),
%  write('Val = '),write(Val),write(', '),write('Player = '),write(Player),nl,
 !.

alphabeta(_/Board, Player, Bounds, GoodMove, GoodVal, Depth) :-
  nb_getval(branches, N), NC is N+1, nb_setval(branches, NC),
  board:moves(Board, Moves),
  OneDeeper is Depth - 1,
  boundedbest(Board, Moves, Player, Bounds, GoodMove, GoodVal, OneDeeper), !.

boundedbest(Board, [Move|TailMoves], Player, Bounds, GoodMove, GoodVal, Depth) :-
  moves:put(Board, Move, Player, NewBoard),
  opponent(Player, Opponent),
  alphabeta(Move/NewBoard, Opponent, Bounds, _, Val, Depth),
  goodenough(Board, TailMoves, Player, Bounds, Move, Val, GoodMove, GoodVal, Depth),
%  write('GoodMove = '),write(GoodMove),write(', '),write('GoodVal = '),write(GoodVal),write(', '),write('Player = '),write(Player),nl,
!.

goodenough(_, [], _, _, Move, Val, Move, Val, _) :- !. % No other candidate

goodenough(_, _, Player, Alpha/Beta, Move, Val, Move, Val, _) :-
  min_to_move(Player), Val > Beta, !
  ;
  max_to_move(Player), Val < Alpha, !.

goodenough(Board, TailMoves, Player, Bounds, Move, Val, GoodMove, GoodVal, Depth)  :-
  newbounds(Bounds, Player, Val, NewBounds),
  boundedbest(Board, TailMoves, Player, NewBounds, NewMove, NewVal, Depth),
  betterof(Player, NewMove, NewVal, Move, Val, GoodMove, GoodVal), !.

newbounds(Alpha/Beta, Player, Val, Val/Beta) :-
  min_to_move(Player), Val > Alpha, !.

newbounds(Alpha/Beta, Player, Val, Alpha/Val) :-
  max_to_move(Player), Val < Beta, !.

newbounds(Alpha/Beta, _, _, Alpha/Beta) :- !.

betterof(Player, Move1, Val1, _, Val2, Move1, Val1) :-
  min_to_move(Player), Val1 > Val2, !
  ;
  max_to_move(Player), Val1 < Val2, !.

betterof(_, _, _, Move2, Val2, Move2, Val2) :- !.

max_to_move(X) :- board:me(X), !.
min_to_move(X) :- \+ max_to_move(X), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(minimax).

test(first_move) :-
  board:empty_board(Em),
  minimax(Em, x, [Z,Y,X], Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl.

test(second_move) :-
  Board =
    0 / 0 / 0 / x /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / o / 0 / 0 /
    0 / 0 / 0 / 0 /

    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0,

  minimax(Board, x, [Z,Y,X], Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl.

test(win_next) :-
  Board =
    o / o / o / 0 /
    o / 0 / 0 / o /
    o / 0 / o / o /
    0 / o / o / o /

    x / x / 0 / 0 /
    0 / 0 / 0 / 0 /
    x / x / x / 0 /
    0 / 0 / 0 / 0 /

    0 / x / x / 0 /
    0 / 0 / 0 / 0 /
    0 / x / 0 / 0 /
    x / 0 / 0 / 0 /

    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0,

  minimax(Board, x, [Z,Y,X], Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl,
  [Z,Y,X] = [2,2,3].

test(third_move) :-
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
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    x / 0 / 0 / 0 /
    x / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0,

  minimax(Board, x, [Z,Y,X], Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl,
  ([Z,Y,X] = [0,2,0] ; [Z,Y,X] = [0,3,0]).

test(block_fork) :-
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
    x / o / 0 / 0 /
    o / 0 / 0 / o /
    0 / 0 / 0 / 0 /

    0 / x / 0 / 0 /
    x / x / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0,

  minimax(Board, x, [Z,Y,X], Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl,
  ([Z,Y,X] = [1,2,2]).

test(fork) :-
  Board =
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0 /

    x / 0 / 0 / 0 /
    o / x / 0 / 0 /
    x / 0 / 0 / x /
    0 / 0 / 0 / 0 /

    0 / o / 0 / 0 /
    o / o / 0 / 0 /
    0 / 0 / 0 / 0 /
    0 / 0 / 0 / 0,

  minimax(Board, x, [Z,Y,X], Val, 2),
  nb_getval(branches, N),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl,
  ([Z,Y,X] = [1,2,2]).

:- end_tests(minimax).

