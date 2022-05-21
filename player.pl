:- dynamic players/2.

% Predicado dinamico players con 2 argumentos, el 1ro sera el numero del jugador y el 2do la puntuacion del mismo
% N -> Numero del jugador
% S -> Puntuacion del jugador

% Agrega la clausula a player de un nuevo jugador numero N y con puntuacion=0
create_player(N):-
    assert(players(N, 0)).

create_players(1):-
    create_player(1).
create_players(N):-
    N > 1,
    N1 is N-1,
    create_player(N),
    create_players(N1).

remove_players():-
    retractall(players(_,_)).

%Score
set_score(Player, Score):-
    retract(players(Player, _)),
    assert(players(Player, Score)).

add_score(Player, Score_to_Add):-
    retract(players(Player, Score)),
    S is Score+Score_to_Add,
    assert(players(Player, S)).

get_score(Player, Score):-
    players(Player, Score).


