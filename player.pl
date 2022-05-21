:- dynamic players/9.

% Predicado dinamico players con 9 argumentos, el 1ro sera el numero del jugador y el 2do la puntuacion del mismo
% N -> Numero del jugador
% S -> Puntuacion del jugador

% Agrega la clausula a player de un nuevo jugador numero N y con puntuacion=0
create_player(N):-
    assert(players(N, 0, (0,0), (0,0), (0,0), (0,0), (0,0), [], [] )),!. % Agregamos una clausula nueva al predicado players

% Caso base, se crea 1 jugador
create_players(1):-
    create_player(1),!.

% Creando N jugadores
create_players(N):-
    N > 1,
    N1 is N-1,
    create_player(N),
    create_players(N1),!.

% Elimina a todos los jugadores que se encuentran en players
remove_players():-
    retractall(players(_,_,_,_,_,_,_,_,_)).

% Score
% Asigna Una puntuacion a un jugador con indice Player
set_score(Player, Score):-
    retract(players(Player, _, R1, R2,R3,R4,R5, W, B)),
    assert(players(Player, Score, R1, R2,R3,R4,R5, W, B)).

% Aumenta la puntuacion del jugador con indice Player
add_score(Player, Score_to_Add):-
    retract(players(Player, Score,R1, R2,R3,R4,R5, W, B)),
    S is Score+Score_to_Add,
    assert(players(Player, S, R1, R2,R3,R4,R5, W, B)).

concatList([], Z, Z).
concatList([A|X], Y, [A|Z]) :- 
    concatList(X, Y, Z).


kk:-
    create_players(2),
   % players(X,Y,(Z,W), R1,R2,R3,R4, W, B),
    %format("Fin del Turno. ~a ~a ~a ~a ~n",[X,Y,Z,W]),
    concatList([[0,1],[2,3]], [2], V),
    print(V).


kk.