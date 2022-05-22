:-[wall].

% Predicado dinamico players con 9 argumentos, el 1ro sera el numero del jugador y el 2do la puntuacion del mismo
% N -> Numero del jugador
% S -> Puntuacion del jugador
% R1 -> Piso 1 de la escalera          _
% R2 -> Piso 2 de la escalera        _ _
% R3 -> Piso 3 de la escalera      _ _ _
% R4 -> Piso 4 de la escalera    _ _ _ _
% R5 -> Piso 5 de la escalera  _ _ _ _ _
% M -> Mosaico o Matriz de la derecha o Muro
% D -> Fichas descartadas(Dropped Tiles)
:- dynamic players/9, 
    first_player/1. % Jugador que comenzo jugando esta ronda(Tenia la ficha 1)

update_first_player(P):-
    retractall(first_player(_)),
    assert(first_player(P)).

% Agrega la clausula a player de un nuevo jugador numero N y con puntuacion=0
create_player(N):-
    assert(players(N, 0, (0,0), (0,0), (0,0), (0,0), (0,0), 
    [
        [0,0,0,0,0],
        [0,0,0,0,0],
        [0,0,0,0,0],
        [0,0,0,0,0],
        [0,0,0,0,0]
    ]
    , 0 )),!. % Agregamos una clausula nueva al predicado players

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
    retract(players(Player, _, R1, R2,R3,R4,R5, M, D)),
    assert(players(Player, Score, R1, R2,R3,R4,R5, M, D)).

% Aumenta la puntuacion del jugador con indice Player
add_score(Player, Score_to_Add):-
    retract(players(Player, Score,R1, R2,R3,R4,R5, M, D)),
    S is Score+Score_to_Add,
    assert(players(Player, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 1 de la escalera
update_R1(P, R1):-
    retract(players(Player, S, _, R2,R3,R4,R5, M, D)),
    assert(players(Player, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 2 de la escalera
update_R2(P, R2):-
    retract(players(Player, S, R1, _,R3,R4,R5, M, D)),
    assert(players(Player, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 3 de la escalera
update_R3(P, R3):-
    retract(players(Player, S, R1, R2, _,R4,R5, M, D)),
    assert(players(Player, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 4 de la escalera
update_R4(P, R4):-
    retract(players(Player, S, R1, R2, R3,_,R5, M, D)),
    assert(players(Player, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 5 de la escalera
update_R5(P, R5):-
    retract(players(Player, S, R1, R2, R3,R4,_, M, D)),
    assert(players(Player, S, R1, R2,R3,R4,R5, M, D)).


%assert(colors(1,'negro')),
%assert(colors(2,'azul')),
%assert(colors(3,'amarillo')),
%assert(colors(4,'rojo')),
%assert(colors(5,'blanco')).

% Ubica las fichas(A=cantidad de fichas) de color C en el jugador P en la fila R
% P -> Player
% C -> Color
% A -> Amount
% R -> Row
update_row(P, C, A, 1):-
    update_R1(P, (C,A))

% Dice si es posible ubicar las fichas(A=cantidad de fichas) de color C en el jugador P en la fila R
% P -> Player
% C -> Color
% A -> Amount
% R -> Row
% NewA -> En caso de poder actualizarse cual seria la nueva cantidad, de no poder actualizarse la cantidad seria 0
can_set_tiles_in_row(P,C, A, 1):-
    players(P, _, R1, _, _, _, _, _, _),


% Calcular la puntuacion a restar con n fichas descartadas.
% N numero de fichas descartadas(se incluye la ficha de jugadr inicial)
% S valor en numeros negativos a descontar o 0 si N es 0
calculateDropScore(0, 0).
calculateDropScore(1, -1).
calculateDropScore(2, -2).
calculateDropScore(3, -4).
calculateDropScore(4, -6).
calculateDropScore(5, -8).
calculateDropScore(6, -11).
calculateDropScore(7, -14).
calculateDropScore(N, -14) :-
    N > 7.

% Actualizar cantidad de piezas descartadas
update_dropped_tiles(P, D):-
    retract(players(P, Score,R1, R2,R3,R4,R5, M, _)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar matriz de la derecha(Muro)
update_matrix(P, M):-
    retract(players(P, Score,R1, R2,R3,R4,R5, _, D)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).





kk:-
    create_players(2),
   % players(X,Y,(Z,W), R1,R2,R3,R4, W, B),
    %format("Fin del Turno. ~a ~a ~a ~a ~n",[X,Y,Z,W]),
    %cell_values([[0,0], [0,1], [1,0], [1,1]], [[6,4],[2,3]], V),
    %print(V).

    
    %update_mat_rc([[1,2,3],[4,5,6],[7,8,9]],1,1,27,Z),
    %M = [[1, 2, 3], [4, x, 6], [7, 8, 9]].
    %print(Z),

    %nth0(2, Z, O),
    %print(O).

kk.