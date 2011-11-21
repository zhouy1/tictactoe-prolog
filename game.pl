:- module(game,
        [ play/3                 % The 4x4x4 tic-tac-toe computer player
        ]).

:- use_module(minimax,
        [ minimax/5
        ]).

:- use_module(board,
        [ empty_board/1,
          opponent/2,
          print_board/1
        ]).

:- use_module(moves,
        [ put/4
        ]).

:- use_module(heuristics,
        [ h_func/4
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Game play
%%%
%%%
%%%     The play/3 function runs the I/O interaction.
%%%
%%%         play(+In, +Out, +Initiate)
%%%
%%%         where In is the input descriptor
%%%               Out is the output descriptor
%%%               Initiate should be yes or no, and indicates if
%%%                        it should initiate the game.

depth(4).

play(In, Out, yes) :-
  depth(Depth), board:empty_board(Board), me(X),
  minimax:minimax(Board, X, Move, NextBoard, Depth),
  send_move(In, Out, Move),
  play(In, Out, Depth, NextBoard, X).

play(In, Out, no) :-
  depth(Depth), board:empty_board(Board), me(X),
  play(In, Out, Depth, Board, X).

play(In, Out, Depth, Board, Me) :-
  receive_move(In, Out, Board, OtherBoard),
  write('--- Thinking...'), nl,
  minimax(OtherBoard, Me, Move, NextBoard, Depth),
  send_move(In, Out, NextBoard, Move),
  play(In, Out, Depth, NextBoard, Me).

send_move(In, Out, Board, [Z,Y,X]) :-
  format('--- My move was ~d/~d/~d.', [X,Y,Z]), nl,
  board:print_board(Board),
  write(Out, jogada(X,Y,Z)), write(Out, .), nl(Out),
  flush_output(Out),
  read(In, Response),
  (
    Response = aceita ->
      write('--- Sent move.') ;
    Response = recusada -> fail ;
      send_move(In, Out, Board, [Z,Y,X])
  ).

receive_move(In, Out, Board, OtherBoard) :-
  (
    (read(In, jogada(X,Y,Z)), is_valid(Board, [Z,Y,X])) ->
      (
        write(Out, aceita), write(Out, .), nl(Out),
        flush_output(Out),
        format('--- Opponent\'s move was ~d/~d/~d.', [X,Y,Z]), nl,
        me(Me), board:opponent(Me, Other),
        moves:put(Board, [Z,Y,X], Other, OtherBoard),
        board:print_board(OtherBoard)
      ) ;
      (
        write(Out, recusada), write(Out, .), nl(Out),
        flush_output(Out),
        write('>>> ERROR: Refused player game, trying again...'), nl,
        receive_move(In, Out, X, Y, Z)
      )
  ).

is_valid(Board, [Z,Y,X]) :-
  X >= 0, X =< 3,
  Y >= 0, Y =< 3,
  Z >= 0, Z =< 3,
  % the position should be empty
  me(Me), board:opponent(Me, Other),
  heuristics:h_func(Board, [Z,Y,X], Other, W),
  W > 0.

