    % PPPPPPPP    ll                                                        ll  
    % PP    PP    ll                                                        ll  
    % PP    PP    ll    aaaaaa  yy      yy  eeeeee    rrrrrr      pppppp    ll  
    % PP    PP    ll    aa  aa    yy  yy    ee    ee  rr          pp    pp  ll  
    % PPPPPPPP    ll    aaaaaa    yy  yy    eeeeeeee  rr          pp    pp  ll  
    % PP          ll    aa  aa    yy  yy    ee        rr          pp    pp  ll  
    % PP          ll    aa  aa      yy      ee        rr          pp    pp  ll  
    % PP          llll  aaaaaa      yy      eeeeee    rr      ..  pppppp    llll
    %                               yy                            pp            
    %                             yy                              pp            

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
% D -> Cantidad de Fichas descartadas(Dropped Tiles)
:- dynamic players/9, 
    first_player/1. % Jugador que comenzo jugando esta ronda(Tenia la ficha 1)

update_first_player(P):-
    assert(first_player(_)),
    retractall(first_player(_)),
    assert(first_player(P)).

% Agrega la clausula a player de un nuevo jugador numero N y con puntuacion=0
create_player(N):-
    assert(players(N, 0, (0,0), (0,0), (0,0), (0,0), (0,0), 
    [
        [1,1,1,1,1],
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
set_score(P, Score):-
    retract(players(P, _, R1, R2,R3,R4,R5, M, D)),
    assert(players(P, Score, R1, R2,R3,R4,R5, M, D)).

% Aumenta la puntuacion del jugador con indice Player
add_score(P, Score_to_Add):-
    retract(players(P, Score,R1, R2,R3,R4,R5, M, D)),
    S is Score+Score_to_Add,
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 1 de la escalera
update_R1(P, R1):-
    retract(players(P, S, _, R2,R3,R4,R5, M, D)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 2 de la escalera
update_R2(P, R2):-
    retract(players(P, S, R1, _,R3,R4,R5, M, D)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 3 de la escalera
update_R3(P, R3):-
    retract(players(P, S, R1, R2, _,R4,R5, M, D)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 4 de la escalera
update_R4(P, R4):-
    retract(players(P, S, R1, R2, R3,_,R5, M, D)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).

% Actualizar Piso 5 de la escalera
update_R5(P, R5):-
    retract(players(P, S, R1, R2, R3,R4,_, M, D)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).


%assert(colors(1,'negro')),
%assert(colors(2,'azul')),
%assert(colors(3,'amarillo')),
%assert(colors(4,'rojo')),
%assert(colors(5,'blanco')).

% Ubica las fichas(A=cantidad de fichas) de color C en el jugador P en la fila R
% P -> Player
% C -> Color
% A -> Amount = Cantidad
% R -> Row = 1
update_row(P, C, A, 1):-
    update_R1(P, (C,A)).

% Lo mismo que el anterior, pero Row=2
% Row = 2
update_row(P, C, A, 2):-
    update_R1(P, (C,A)).

% Lo mismo que el anterior, pero Row=3
% Row = 3
update_row(P, C, A, 3):-
    update_R1(P, (C,A)).

% Lo mismo que el anterior, pero Row=4
% Row = 4
update_row(P, C, A, 4):-
    update_R1(P, (C,A)).

% Lo mismo que el anterior, pero Row=5
% Row = 5
update_row(P, C, A, 5):-
    update_R1(P, (C,A)).

%devuelve la tupla en la fila 1,2,3,4,5 respectivamente del jugador player 
get_row_1(P,Result):- players(P,_,Result,_,_,_,_,_,_).
get_row_2(P,Result):- players(P,_,_,Result,_,_,_,_,_).
get_row_3(P,Result):- players(P,_,_,_,Result,_,_,_,_).
get_row_4(P,Result):- players(P,_,_,_,_,Result,_,_,_).
get_row_5(P,Result):- players(P,_,_,_,_,_,Result,_,_).

%obtiene la tupla de la fila n
%P Player
get_row_n(P,1,Result):-get_row_1(P,Result).
get_row_n(P,2,Result):-get_row_2(P,Result).
get_row_n(P,3,Result):-get_row_3(P,Result).
get_row_n(P,4,Result):-get_row_4(P,Result).
get_row_n(P,5,Result):-get_row_5(P,Result).


% Dice si es posible ubicar las fichas(A=cantidad de fichas) de color C en el jugador P en la fila R
% P -> Player
% C -> Color
% A -> Amount
% R -> Row = 1
% NewA -> En caso de poder actualizarse cual seria la nueva cantidad, de no poder actualizarse la cantidad seria 0
can_set_tiles_in_row(P,C, A, 1, NewA):-
    players(P, _, R1, _, _, _, _, W, _),
    (C1,A1) = R1,
    set_dynamic_bool_false,
    not(color_in_row(C, 1, W, R)),
    % print(R),
    % R =:= 0,
    % not(R), % El color no se encuentra en la fila del Muro
    format("~a ~a ~n", [C1, A1]),
    (C1 = 0; C1 = C),
    % A + A1 =< 1,
    % format("Se va a poner en true el dynamic bool ~n"),
    set_dynamic_bool_true,
    A2 is 1 - A1,
    NewA is A - A2.
    % format("NewA es ~a ~n ",[NewA]).
can_set_tiles_in_row(P,C, A, 1, NewA).

% Lo mismo que el anterior, pero Row=2
% Row = 2
can_set_tiles_in_row(P,C, A, 2, NewA):-
    players(P, _,  _, R2, _, _, _, W, _),
    (C1,A1) = R2,
    set_dynamic_bool_false,
    not(color_in_row(C, 2, W, R)), % El color no se encuentra en la fila del Muro
    format("~a ~a ~n", [C1, A1]),
    (C1 = 0; C1 = C),
    % A + A1 =< 1,
    % format("Se va a poner en true el dynamic bool ~n"),
    set_dynamic_bool_true,
    A2 is 2 - A1,
    NewA is A - A2.
    % format("NewA es ~a ~n",[NewA]).
can_set_tiles_in_row(P,C, A, 2, NewA).
% Lo mismo que el anterior, pero Row=3
% Row = 3
can_set_tiles_in_row(P,C, A, 3, NewA):-
    players(P, _, _,_,R3,  _, _, W, _),
    (C1,A1) = R3,
    set_dynamic_bool_false,
    not(color_in_row(C, 3, W, R)), % El color no se encuentra en la fila del Muro
    format("~a ~a ~n", [C1, A1]),
    (C1 = 0; C1 = C),
    % A + A1 =< 1,
    % format("Se va a poner en true el dynamic bool ~n"),
    set_dynamic_bool_true,
    A2 is 3 - A1, % A2 = Cantidad restante que se pueden ubicar en la fila R3
    NewA is A - A2.
    % format("NewA es  ~a ~n",[NewA]).
can_set_tiles_in_row(P,C, A, 3, NewA).
% Lo mismo que el anterior, pero Row=4
% Row = 4
can_set_tiles_in_row(P,C, A, 4, NewA):-
    players(P, _, _,_,_, R4, _, _, _),
    (C1,A1) = R4,
    set_dynamic_bool_false,
    not(color_in_row(C, 4, W, R)), % El color no se encuentra en la fila del Muro
    format("~a ~a ~n", [C1, A1]),
    (C1 = 0; C1 = C),
    % A + A1 =< 1,
    % format("Se va a poner en true el dynamic bool ~n"),
    set_dynamic_bool_true,
    A2 is 4 - A1,
    NewA is A - A2.
    % format("NewA es ~a ~n ",[NewA]).
can_set_tiles_in_row(P,C, A, 4, NewA).

% Lo mismo que el anterior, pero Row=5
% Row = 5
can_set_tiles_in_row(P,C, A, 5, NewA):-
    players(P, _, _,_,_, _, R5, _, _),
    (C1,A1) = R5,
    set_dynamic_bool_false,
    not(color_in_row(C, 5, W, R)), % El color no se encuentra en la fila del Muro
    format("~a ~a ~n", [C1, A1]),
    (C1 = 0; C1 = C),
    % A + A1 =< 1,
    % format("Se va a poner en true el dynamic bool ~n"),
    set_dynamic_bool_true,
    A2 is 5 - A1,
    NewA is A - A2.
    % format("NewA es  ~a ~n",[NewA]).
can_set_tiles_in_row(P,C, A, 5, NewA).

% Calcular la puntuacion a restar con n fichas descartadas.
% N numero de fichas descartadas(se incluye la ficha de jugadr inicial)
% S valor en numeros negativos a descontar o 0 si N es 0
get_negative_score(0,  0).
get_negative_score(1, -1).
get_negative_score(2, -2).
get_negative_score(3, -4).
get_negative_score(4, -6).
get_negative_score(5, -8).
get_negative_score(6, -11).
get_negative_score(7, -14).
get_negative_score(N, -14) :-
    N > 7.

% Devuelve la puntuacion a restar del jugador con indice P
% P -> Numero del jugador
% S valor en numeros negativos a descontar o 0 si N es 0
get_negative_score_for_player(P, S):-
    players(P, _, _, _,_,_,_, _, D),
    get_negative_score(D, S).

% Actualizar cantidad de piezas descartadas
update_dropped_tiles(P, D):-
    retract(players(P, Score,R1, R2,R3,R4,R5, M, _)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).

% El jugador descarte una ficha
% P -> Inndice del jugador
% D1 -> Cantidad de fichas que ha descartado luego de descartar esta ultima
drop_tile(P, D1) :-
    players(P, _, D, _,_,_,_,_,_),
    D1 =\= 0,
    !.
    players(P, _, _, _,_,_,_,_,D),
    D1 is D + 1,
    update_dropped_tiles(P, D1),
    !.

% El jugador descarta 1 fichas(caso base de descartar n fichas)
% P -> Inndice del jugador
% 1 -> Cantidad de fichas a descartar
% D1 -> Cantidad de fichas que ha descartado luego de descartar esta ultima
drop_tiles(P, 1, D1) :-
    drop_tile(P, D1),
    !.

% El jugador descarta N fichas
% P -> Indice del jugador
% N -> Cantidad de fichas a descartar
% D1 -> Cantidad de fichas que ha descartado luego de descartar esta ultima
drop_tiles(P, N, D1) :-
    N > 1,
    N1 is N - 1,
    drop_tiles(P, N1, _),
    drop_tile(P, D1),
    !.

% Actualizar matriz de la derecha(Muro)
update_matrix(P, M):-
    retract(players(P, Score,R1, R2,R3,R4,R5, _, D)),
    assert(players(P, S, R1, R2,R3,R4,R5, M, D)).

kk:-
    create_players(2),
    players(1,_,_,_,_,_,_,M,_),
    % set_temp_score,
    % calculate_row_score(0,1,M, R),
    % print(R).
    % row_is_filled(0, M).
    % calculate_rows_filled_amount_score(M, S),
    diag_is_filled(1, M).
    % print(S).
    % players(1,_,_,_,_,_,_,_,_),
    % players(4,_,_,_,_,_,_,_,_),
    % start_dynamic_bool,
    % dynamic_bool(C),
    % print(C),
    % can_set_tiles_in_row(1,1, 1, 1, NewA),
    % print(NewA),
    % dynamic_bool(B),
    % print(B),

    % update_row(1,1,1,1),

    % can_set_tiles_in_row(1,1, 1, 1, NewB),
    % print(NewB),
    % dynamic_bool(B),
    % print(B).
    % get_col("rojo", 1, X),
    % print(X),
    % valid_pos(1, 3).
    % can_set_tiles_in_row(P,C, A, 3, NewA)
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