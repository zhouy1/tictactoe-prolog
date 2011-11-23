:- module(game,
        [ go/2,                  % the game engine
          play/5                 % The 4x4x4 tic-tac-toe computer player
        ]).

:- use_module(algorithms,
        [ minimax/5
        ]).

:- use_module(board,
        [ empty_board/1,
          opponent/1,
          print_board/1,
          put/4
        ]).

:- use_module(heuristics,
        [ win/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Game play
%%%
%%%
%%%     The game/3 function runs the I/O interaction.
%%%
%%%         game(+In/Out, +Initiate)
%%%
%%%         where In is the input descriptor
%%%               Out is the output descriptor
%%%               Initiate should be yes or no, and indicates if
%%%                        it should initiate the game.

go(I/O, no) :-
  board:empty_board(Board),
  opponent_turn(I/O, Board, 1), !.

go(I/O, yes) :-
  board:empty_board(Board),
  write('--- Yew! Initiating the game!'), nl,
  my_turn(I/O, Board, 1), !.

opponent_turn(I/O, Board, N) :-
  opponent_move(I/O, Board, OpponentBoard, N),
  (
    opponent_wins(OpponentBoard),
    format('<<< [~d] I loose. :(',[N]),nl
    ;
    draw(OpponentBoard),
    format('<<< [~d] Draw...',[N]),nl
    ;
    N1 is N+1,
    my_turn(I/O, OpponentBoard, N1)
  ), !.

my_turn(I/O, Board, N) :-
  my_move(I/O, Board, MyBoard, N),
  (
    i_win(MyBoard),
    format('<<< [~d] I win!',[N]),nl
    ;
    draw(MyBoard),
    format('<<< [~d] Draw...',[N]),nl
    ;
    N1 is N+1,
    opponent_turn(I/O, MyBoard, N1)
  ), !.

i_win(Board) :-
  board:me(X), heuristics:win(Board, X).

opponent_wins(Board) :-
  board:opponent(X), win(Board, X).

draw(Board) :-
  board:moves(Board,[]).

my_move(I/O, Board, MyBoard, N) :-
  write('--- Thinking...'), nl,
  play(Board, MyMove, Val, Branches, Time),
  format('Val = ~d, Branches = ~d, Time = ~1fms.', [Val,Branches,Time]), nl,
  [X,Y,Z] = MyMove,
  format('--- [~d] My move: ~d/~d/~d', [N,X,Y,Z]), nl,
  board:me(Me),
  board:put(Board, MyMove, Me, MyBoard),
  nl,board:print_board(MyBoard),
  send_move(I/O, MyMove), !.

opponent_move(I/O, Board, OpponentBoard, N) :-
  write('--- Waiting for opponent move...'), nl,
  receive_move(I/O, Board, OpponentMove),
  [Z,Y,X] = OpponentMove,
  format('--- [~d] Opponent move: ~d,~d,~d', [N,Z,Y,X]), nl,
  board:opponent(Opponent),
  board:put(Board, OpponentMove, Opponent, OpponentBoard),
  nl,board:print_board(OpponentBoard), !.

play(Board, Move, Val, 0, 0) :-
  board:empty_board(Board),
  heuristics:first_move(Move, Val), !.

play(Board, Move, Val, Branches, Time) :-
  statistics(process_cputime, BeforeCpu),
  (
    algorithms:minimax(Board, Move, Val, Branches, 2)
    ;
    algorithms:minimax(Board, Move, Val, Branches, 1)
  ),
  statistics(process_cputime, AfterCpu),
  Time is (AfterCpu - BeforeCpu) * 1000, !.

send_move(In/Out, [Z,Y,X]) :-
  write('--- Sending move...'), nl,
  write(Out, jogada(X,Y,Z)), write(Out, .), nl(Out),
  flush_output(Out),
  write('--- Waiting for acknowledgement...'), nl,
  read(In, Response),
  (
    Response = aceita, write('--- Move accepted.'), nl
    ;
    write('>>> ERROR: Refused game or invalid response, aborting...'), nl, fail
  ), !.

receive_move(In/Out, Board, [Z,Y,X]) :-
  read(In, jogada(X,Y,Z)),
  is_valid(Board, [Z,Y,X]) ->
  (
    write(Out, aceita), write(Out, .), nl(Out),
    flush_output(Out)
  ), ! ;
  (
    write(Out, recusada), write(Out, .), nl(Out),
    flush_output(Out),
    format('>>> ERROR: Invalid move ~d,~d,~d, trying again...', [Z,Y,X]), nl,
    receive_move(In/Out, Board, [Z,Y,X])
  ), !.

is_valid(Board, [Z,Y,X]) :-
  X >= 0, X =< 3,
  Y >= 0, Y =< 3,
  Z >= 0, Z =< 3,
  % the position should be empty
  board:is_empty(Board, [Z,Y,X]).

debug_play(Board) :-
  debug_play(Board, _), !.

debug_play(Board, [Z,Y,X]) :-
  play(Board, [Z,Y,X], Val, Branches, Time),
  nl, format('Move: ~d/~d/~d, Val = ~d, Branches = ~d, Time = ~1fms.', [Z,Y,X,Val,Branches,Time]), nl,
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(game).

test(first_move) :-
  board:empty_board(Em),
  debug_play(Em).

test(second_move) :-
  debug_play(
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
    0 / 0 / 0 / 0
  ).

test(win_next) :-
  debug_play(
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

  [Z,Y,X]), [Z,Y,X] = [2,2,3].

test(third_move) :-
  debug_play(
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

  [Z,Y,X]), ([Z,Y,X] = [0,2,0] ; [Z,Y,X] = [0,3,0]).

test(block_fork) :-
  debug_play(
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

  [Z,Y,X]), [Z,Y,X] = [1,2,2].

test(fork) :-
  debug_play(
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

  [Z,Y,X]), [Z,Y,X] = [1,2,2].

test(last_move) :-
  debug_play(
    o / o / o / x /
    o / x / x / o /
    x / x / o / o /
    o / x / o / o /

    x / x / o / x /
    o / o / o / x /
    x / x / x / o /
    o / o / x / x /

    x / x / x / o /
    o / o / o / x /
    o / x / o / x /
    x / x / o / x /

    o / o / o / x /
    x / x / o / 0 /
    x / o / x / o /
    o / x / o / x,

  [Z,Y,X]), [Z,Y,X] = [0,1,3].

test(draw) :-
  debug_play(
    o / o / o / x /
    o / x / x / o /
    x / x / o / o /
    o / x / o / o /

    x / x / o / x /
    o / o / o / x /
    x / x / x / o /
    o / o / x / x /

    x / x / x / o /
    o / o / o / x /
    o / x / o / x /
    x / x / o / x /

    o / o / o / x /
    x / x / o / o /
    x / o / x / o /
    o / x / o / x), fail ; true.

:- end_tests(game).

