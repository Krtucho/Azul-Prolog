%                         ii    ll                                        ll    
%                 tt            ll                                        ll    
%                 tt            ll                                        ll    
%   uu      uu  tttttttt  ii    ll      ssssssss            pp  pppppp    ll    
%   uu      uu    tt      ii    ll      ss    ssss          pppp    pp    ll    
%   uu      uu    tt      ii    ll      ss                  pp      pp    ll    
%   uu      uu    tt      ii    ll      ssssss              pp      pp    ll    
%   uu      uu    tt      ii    ll            ssss          pp      pp    ll    
%   uu      uu    tt      ii    ll              ss          pp      pp    ll    
%   uu      uu    tt      ii    ll    ssss      ss          pp      pp    ll    
%     uuuuuuuu      tttt  ii    llllll  ssssssss      ..    pppppppp      llllll
%                                                           pp                  
%                                                           pp                  
%                                                           pp                  


concatList([], Z, Z).
concatList([A|X], Y, [A|Z]) :- 
    concatList(X, Y, Z).

%S: 
%[ 5   6 
%  3   4 ] 
% Represented in prolog as: S = [ [5,6], [3,4]].

%I'm trying to write a recursive function to get a cell value such that cell_values (Cells, Matrix, Values) would return a list of the values from a list of the cell.
%Example: cell_values ([[0,0], [0,1]], S, Values) --> Values = [5, 6]. Where S is the matrix above.
% R -> 1st value(position x,y) to find
% C -> 2nd value(position x,y) to find
% T -> nth value(position x,y) to find
% M -> Matrix
% V -> 1st value found
% VT -> nth value found
get_values([], _, []).
get_values([[R, C]| T], M, [V|VT]) :-
    nth0(R, M, Row),
    nth0(C, Row, V),
    get_values(T, M, VT).

%?- update_mat_rc([[1,2,3],[4,5,6],[7,8,9]],1,1,x,M).
%M = [[1, 2, 3], [4, x, 6], [7, 8, 9]].
update_mat_rc(Mc,R,C,V,Mu) :-
    nth0(R,Mc,Rc,Mt),
    nth0(C,Rc,_,Rt),
    nth0(C,Ru,V,Rt),
    nth0(R,Mu,Ru,Mt).

% Index -> Posicion de la lista a buscar
% Matrix(Matriz o lista en la que vamos a buscar)
% Fila resultante
get_row(Index, Matrix, Row):-
    nth0(Row, Matrix, Row).

%?- get_col( [[1,2], [3, 4], [5,6]], 0, Col).
%Col = [1, 3, 5].

%?- get_col( [[1,2], [3, 4], [5,6]], I, Col).
%I = 0,
%Col = [1, 3, 5] ;
%I = 1,
%Col= [2, 4, 6].
get_col([], _, []).
get_col([X|Xs], I, [Y|Ys]) :-
    nth0(I, X, Y),
    get_col(Xs, I, Ys).

%                         ii    ll                                        ll    
%                 tt            ll                                        ll    
%                 tt            ll                                        ll    
%   uu      uu  tttttttt  ii    ll      ssssssss            pp  pppppp    ll    
%   uu      uu    tt      ii    ll      ss    ssss          pppp    pp    ll    
%   uu      uu    tt      ii    ll      ss                  pp      pp    ll    
%   uu      uu    tt      ii    ll      ssssss              pp      pp    ll    
%   uu      uu    tt      ii    ll            ssss          pp      pp    ll    
%   uu      uu    tt      ii    ll              ss          pp      pp    ll    
%   uu      uu    tt      ii    ll    ssss      ss          pp      pp    ll    
%     uuuuuuuu      tttt  ii    llllll  ssssssss      ..    pppppppp      llllll
%                                                           pp                  
%                                                           pp                  
%                                                           pp                  


% WW      WW      WW              ll    ll                      ll  
% WW      WW      WW              ll    ll                      ll  
%   WW    WW      WW    aaaaaa    ll    ll          pppppppp    ll  
%   WW  WW  WW  WW    aa    aa    ll    ll          pp    pp    ll  
%   WW  WW  WW  WW          aa    ll    ll          pp    pp    ll  
%   WW  WW  WW  WW    aaaaaaaa    ll    ll          pp    pp    ll  
%   WW  WW  WW  WW    aa    aa    ll    ll          pp    pp    ll  
%     WW      WW      aa    aa    ll    ll          pp    pp    ll  
%     WW      WW      aaaaaaaa    llll  llll  ..    pppppppp    llll
%                                                   pp              
%                                                   pp              
%                                                   pp              


% Obtiene la columna C de la loza de color T en la fila R del muro
% T -> Type
% R -> Row
% C -> Column
get_col("rojo", 1, 3).
get_col("rojo", 2, 4).
get_col("rojo", 3, 5).
get_col("rojo", 4, 1).
get_col("rojo", 5, 2).

get_col(negro, 1, 4).
get_col(negro, 2, 5).
get_col(negro, 3, 1).
get_col(negro, 4, 2).
get_col(negro, 5, 3).

get_col(amarillo, 1, 2).
get_col(amarillo, 2, 3).
get_col(amarillo, 3, 4).
get_col(amarillo, 4, 5).
get_col(amarillo, 5, 1).

get_col(azul, 1, 1).
get_col(azul, 2, 2).
get_col(azul, 3, 3).
get_col(azul, 4, 4).
get_col(azul, 5, 5).

get_col(blanco, 1, 5).
get_col(blanco, 2, 1).
get_col(blanco, 3, 2).
get_col(blanco, 4, 3).
get_col(blanco, 5, 4).

% Posicion valida en una matriz (Determina si los valores se encuentran del rango de numeros validos en una matriz)
valid_pos(R, C):-
    R >= 0,
    R =< 4,
    C >= 0,
    C =< 4.
% Dada una matriz M nos dice si la casilla (R,C) de la misma esta vacia y si es posible insertar alguna losa de algun color
% R -> Fila
% C -> Columna
% M -> Matriz
% V -> Valor ubicado en la posicion (R,C) de la matriz M
valid_pos_to_insert_tile(R, C, M, V):-
    valid_pos(R,C),
    get_values([R,C], M, V),
    V =:= 0.

% Dada una matriz M inserta en la posicion (R,C) la loseta de color Tile
% R -> Fila
% C -> Columna
% M -> Matriz
% V -> Valor ubicado en la posicion (R,C) de la matriz M
insert_tile(R, C, M, Tile):-
    update_mat_rc(Mc,R,C,Tile,Mu).

% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos horizontalmente(fila) desde esa casilla 
% R -> Fila
% C -> Columna
% M -> Matriz
% S -> Puntuacion
calculate_row_score(R,C, M, S):-
    RL is R + 1,
    RR is R - 1,
    not(valid_pos_to_insert_tile())

% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos verticalmente(columna) desde esa casilla
% R -> Fila
% C -> Columna
% M -> Matriz
% S -> Puntuacion
calculate_column_score(R,C, M, S).

% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos horizontal(fila) y verticalmente(columna) desde esa casilla
% R -> Fila
% C -> Columna
% M -> Matriz
% S -> Puntuacion
calculate_score(R,C, M).

    % WW      WW      WW              ll    ll                      ll  
    % WW      WW      WW              ll    ll                      ll  
    %   WW    WW      WW    aaaaaa    ll    ll          pppppppp    ll  
    %   WW  WW  WW  WW    aa    aa    ll    ll          pp    pp    ll  
    %   WW  WW  WW  WW          aa    ll    ll          pp    pp    ll  
    %   WW  WW  WW  WW    aaaaaaaa    ll    ll          pp    pp    ll  
    %   WW  WW  WW  WW    aa    aa    ll    ll          pp    pp    ll  
    %     WW      WW      aa    aa    ll    ll          pp    pp    ll  
    %     WW      WW      aaaaaaaa    llll  llll  ..    pppppppp    llll
    %                                                   pp              
    %                                                   pp              
    %                                                   pp              
    


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

% Dice si es posible ubicar las fichas(A=cantidad de fichas) de color C en el jugador P en la fila R
% P -> Player
% C -> Color
% A -> Amount
% R -> Row = 1
% NewA -> En caso de poder actualizarse cual seria la nueva cantidad, de no poder actualizarse la cantidad seria 0
can_set_tiles_in_row(P,C, A, 1, NewA):-
    players(P, _, (C1,A1) = R1, _, _, _, _, _, _),
    C1 =:= C,
    A + A1 =< 1,
    NewA is A + A1.

% Lo mismo que el anterior, pero Row=2
% Row = 2
can_set_tiles_in_row(P,C, A, 2, NewA):-
    players(P, _, _, (C1,A1) = R2, _, _, _, _, _),
    C1 =:= C,
    A + A1 =< 1,
    NewA is A + A1.

% Lo mismo que el anterior, pero Row=3
% Row = 3
can_set_tiles_in_row(P,C, A, 3, NewA):-
    players(P, _, _,_, (C1,A1) = R3,  _, _, _, _),
    C1 =:= 0;
    C1 =:= C,
    A + A1 =< 1,
    NewA is A + A1.
% Lo mismo que el anterior, pero Row=4
% Row = 4
can_set_tiles_in_row(P,C, A, 4, NewA):-
    players(P, _, _,_,_, (C1,A1) = R4, _, _, _),
    C1 =:= 0;
    C1 =:= C,
    A + A1 =< 1,
    NewA is A + A1.

% Lo mismo que el anterior, pero Row=5
% Row = 5
can_set_tiles_in_row(P,C, A, 5, NewA):-
    players(P, _, _,_,_, _, (C1,A1) = R5, _, _),
    C1 =:= 0;
    C1 =:= C,
    A + A1 =< 1,
    NewA is A + A1.

% Calcular la puntuacion a restar con n fichas descartadas.
% N numero de fichas descartadas(se incluye la ficha de jugadr inicial)
% S valor en numeros negativos a descontar o 0 si N es 0
get_negative_score(0, 0).
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

% kk:-
%     create_players(2),
%     get_col("azul", 1, X),
%     print(X).
%     % can_set_tiles_in_row(P,C, A, 3, NewA)
%    % players(X,Y,(Z,W), R1,R2,R3,R4, W, B),
%     %format("Fin del Turno. ~a ~a ~a ~a ~n",[X,Y,Z,W]),
%     %cell_values([[0,0], [0,1], [1,0], [1,1]], [[6,4],[2,3]], V),
%     %print(V).

    
%     %update_mat_rc([[1,2,3],[4,5,6],[7,8,9]],1,1,27,Z),
%     %M = [[1, 2, 3], [4, x, 6], [7, 8, 9]].
%     %print(Z),

%     %nth0(2, Z, O),
%     %print(O).

% kk.

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
