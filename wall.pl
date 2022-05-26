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

:-[utils].

% Obtiene la columna C de la loza de color T en la fila R del muro
% T -> Type
% R -> Row
% C -> Column
%           T    R  C
find_col("rojo", 1, 3).
find_col("rojo", 2, 4).
find_col("rojo", 3, 5).
find_col("rojo", 4, 1).
find_col("rojo", 5, 2).

find_col("negro", 1, 4).
find_col("negro", 2, 5).
find_col("negro", 3, 1).
find_col("negro", 4, 2).
find_col("negro", 5, 3).

find_col("amarillo", 1, 2).
find_col("amarillo", 2, 3).
find_col("amarillo", 3, 4).
find_col("amarillo", 4, 5).
find_col("amarillo", 5, 1).

find_col("azul", 1, 1).
find_col("azul", 2, 2).
find_col("azul", 3, 3).
find_col("azul", 4, 4).
find_col("azul", 5, 5).

find_col("blanco", 1, 5).
find_col("blanco", 2, 1).
find_col("blanco", 3, 2).
find_col("blanco", 4, 3).
find_col("blanco", 5, 4).

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

% Devuelve True si se encuentra el color T en la fila R de la matriz W
% T -> Tile o Type o Color de la ficha
% R -> Fila a buscar
% W -> Matriz del jugador 
color_in_row(T, R, W, Result):-
    find_col(T, R, C),
    get_values([R,C], W, V),
    V =:= 1.
    % find_col(T, R, C),
    % Result is True.

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
    not(valid_pos_to_insert_tile()).

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