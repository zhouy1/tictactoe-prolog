:- module(sockets,
        [ create_server/2,       % The server-side function
          create_client/3        % The client-side function
        ]).

:- use_module(game,
        [ play/3
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
  tcp_socket(Socket),
  tcp_bind(Socket, Port), 
  tcp_listen(Socket, 5),
  tcp_open_socket(Socket, AcceptFd, _),
  write('Waiting for connection...'), nl,
  tcp_accept(AcceptFd, Socket, Peer),
  setup_call_cleanup(tcp_open_socket(Socket, In, Out),
                     handle_service(Initiate, Peer, In, Out),
                     close_connection(In, Out)).

close_connection(In, Out) :-
  close(In, [force(true)]),
  close(Out, [force(true)]).

handle_service(Initiate, Peer, In, Out) :-
  format('Received connection from ~a.\n', [Peer]),
  game:play(In, Out, Initiate).

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
  setup_call_catcher_cleanup(tcp_socket(Socket),
                             format('Connecting to ~s:~d...', [Host,Port]), nl,
                             tcp_connect(Socket, Host:Port),
                             exception(_),
                             tcp_close_socket(Socket)),
  setup_call_cleanup(tcp_open_socket(Socket, In, Out),
                     write('Connected!'), nl,
                     game:play(In, Out, Initiate),
                     close_connection(In, Out)).

