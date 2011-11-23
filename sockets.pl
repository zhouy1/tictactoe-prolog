:- module(sockets,
        [ create_server/2,       % The server-side function
          create_client/3        % The client-side function
        ]).

:- use_module(game,
        [ go/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Socket interface to 4x4x4 Tic-Tac-Toe server player
%%%
%%%
%%%     The create_server/2 function starts the server interface.
%%%
%%%         create_server(+Port, +Initiate)
%%%
%%%         where Port is the local port to listen to
%%%               Initiate should be yes or no, and indicates if
%%%                        this player should initiate the game.

create_server(Port, Initiate) :-
  setup_call_cleanup(
    true,
    (
      tcp_socket(Socket),
      tcp_bind(Socket, Port), 
      tcp_listen(Socket, 5),
      tcp_open_socket(Socket, AcceptFd, _),
      write('--- Waiting for connection...'), nl,
      tcp_accept(AcceptFd, Socket2, Peer)
    ),
    tcp_close_socket(AcceptFd)
  ),
  setup_call_cleanup(
    tcp_open_socket(Socket2, In, Out),
    handle_service(Initiate, Peer, In, Out),
    close_connection(In, Out)
  ).

close_connection(In, Out) :-
  close(In, [force(true)]),
  close(Out, [force(true)]).

handle_service(Initiate, Peer, In, Out) :-
  ip(X1,X2,X3,X4) = Peer,
  format('--- Received connection from ~d.~d.~d.~d\n', [X1,X2,X3,X4]),
  game:go(In/Out, Initiate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Socket interface to 4x4x4 Tic-Tac-Toe client player
%%%
%%%
%%%     The create_client/3 function starts the client interface.
%%%
%%%         create_client(+Host, +Port, +Initiate)
%%%
%%%         where Host is the server host
%%%               Port is the server port
%%%               Initiate should be yes or no, and indicates if
%%%                        this player should initiate the game.

create_client(Host, Port, Initiate) :-
  setup_call_catcher_cleanup(
    tcp_socket(Socket),
    (
      format('--- Connecting to ~s:~d...', [Host,Port]), nl,
      tcp_connect(Socket, Host:Port)),
      exception(_),
      tcp_close_socket(Socket)
    ),
  setup_call_cleanup(
    tcp_open_socket(Socket, In, Out),
    (
      write('--- Connected!'), nl,
      game:go(In/Out, Initiate)
    ),
    close_connection(In, Out)
  ).

