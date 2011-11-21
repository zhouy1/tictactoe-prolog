:- module(minimax,
        [ minimax/6            % The minimax algorithm
        ]).

:- use_module(board,
        [ moves/2,
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

minimax(Board, Player, BestMove, BestMove, BestValue, Depth) :-
  Depth > 0,
  nb_setval(branches, 0),
  alphabeta(Board, Player, 0/10000000, BestMove, BestValue, Depth).

value_for(_, _, [], Val, Val).

value_for(Board, Player, [Move|TailMoves], S1, Val) :-
  heuristics:h_func(Board, Move, Player, W),
  S2 is S1+W,
  value_for(Board, Player, TailMoves, S2, Val).

alphabeta(Board, Player, _, _, Val, 0) :-
  opponent(Player, Opponent),
  board:moves(Board, Moves),
  value_for(Board, Player, Moves, 0, MyVal),
  value_for(Board, Opponent, Moves, 0, OpponentVal),
  Val is MyVal-OpponentVal.

alphabeta(Board, Player, Bounds, GoodMove, GoodVal, Depth) :-
  Depth > 0,
  nb_getval(branches, N), NC is N+1, nb_setval(branches, NC),
  board:moves(Board, Moves),
  OneDeeper is Depth - 1,
  boundedbest(Board, Moves, Player, Bounds, GoodMove, GoodVal, OneDeeper).

boundedbest(Board, [Move|TailMoves], Player, Bounds, GoodMove, GoodVal, Depth) :-
  moves:put(Board, Move, Player, NewBoard),
  opponent(Player, Opponent),
  alphabeta(NewBoard, Opponent, Bounds, _, Val, Depth),
  goodenough(Board, TailMoves, Player, Bounds, Move, Val, GoodMove, GoodVal, Depth).

goodenough(_, [], _, _, Move, Val, Move, Val, _).    % No other candidate

goodenough(_, _, Player, Alpha/Beta, Move, Val, Move, Val, _) :-
  min_to_move(Player), Val > Beta, !                 % Maximizer attained upper bound
  ;
  max_to_move(Player), Val < Alpha, !.               % Minimizer attained lower bound

goodenough(Board, TailMoves, Player, Bounds, Move, Val, GoodMove, GoodVal, Depth)  :-
  newbounds(Bounds, Player, Val, NewBounds),         % Refine bounds
  boundedbest(Board, TailMoves, Player, NewBounds, NewMove, NewVal, Depth),
  betterof(Player, NewMove, NewVal, Move, Val, GoodMove, GoodVal).

newbounds(Alpha/Beta, Player, Val, Val/Beta) :-
  min_to_move(Player), Val > Alpha, !.               % Maximizer increased lower bound

newbounds(Alpha/Beta, Player, Val, Alpha/Val) :-
  max_to_move(Player), Val < Beta, !.                % Minimizer decreased upper bound

newbounds(Alpha/Beta, _, _, Alpha/Beta).             % Otherwise bounds unchanged

betterof(Player, Move1, Val1, _, Val2, Move1, Val1) :- % Board1 better than Board2
  min_to_move(Player), Val1 > Val2, !
  ;
  max_to_move(Player), Val1 < Val2, !.

betterof(_, _, _, Move2, Val2, Move2, Val2).         % Otherwise Board2 is better

max_to_move(X) :- me(X).
min_to_move(X) :- \+ max_to_move(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(minimax).
:- use_module(moves, [ put/4 ]).

%test(first_move) :-
%  board:empty_board(Em),
%  minimax(Em, x, [Z,Y,X], _, Val, 2),
%  nb_getval(branches, N),
%  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d.', [Z,Y,X,Val,N]), nl.

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

