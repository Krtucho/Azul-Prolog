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

:-[utils,       % Importando
                % -dynamic_bool/1.
                % -start_dynamic_bool
                % -set_dynamic_bool_true
                % -set_dynamic_bool_false
                % -concatList([], Z, Z).
                % -concatList([A|X], Y, [A|Z]).
                % -get_values([], _, [])
                % -get_values([[R, C]| T], M, [V|VT])
                % -update_mat_rc(Mc,R,C,V,Mu)
game_utils].  
                       

:-dynamic temp_score/1, % Variable dinamica que guarda la puntuacion actual de un jugador que agrega fichas al muro que se esta calculando
            temp_bool_H/1,
            temp_bool_V/1.


% Obtiene la columna C de la loza de color T en la fila R del muro
% T -> Type
% R -> Row
% C -> Column
%           T    R  C
find_col('rojo', 0, 2).
find_col('rojo', 1, 3).
find_col('rojo', 2, 4).
find_col('rojo', 3, 0).
find_col('rojo', 4, 1).

find_col('negro', 0, 3).
find_col('negro', 1, 4).
find_col('negro', 2, 0).
find_col('negro', 3, 1).
find_col('negro', 4, 2).

find_col('amarillo', 0, 1).
find_col('amarillo', 1, 2).
find_col('amarillo', 2, 3).
find_col('amarillo', 3, 4).
find_col('amarillo', 4, 0).

find_col('azul', 0, 0).
find_col('azul', 1, 1).
find_col('azul', 2, 2).
find_col('azul', 3, 3).
find_col('azul', 4, 4).

find_col('blanco', 0, 4).
find_col('blanco', 1, 0).
find_col('blanco', 2, 1).
find_col('blanco', 3, 2).
find_col('blanco', 4, 3).

% Posicion valida en una matriz (Determina si los valores se encuentran del rango de numeros validos en una matriz)
valid_pos(R, C):-
    R >= 0,
    R =< 4,
    C >= 0,
    C =< 4.

% Pone X al valor del predicado temp_score
% Esto ayuda mucho a la hora de contar la puntuacion que gana el jugador al pasar un azulejo hacia el muro 
set_temp_score(X):-
    assert(temp_score(_)),
    retractall(temp_score(_)),
    assert(temp_score(X)).

% Elimina el ultimo valor que tenia el predicado temp_score y le pone el valor 1
clean_temp_score:-
    retractall(temp_score(_)),
    set_temp_score(1).

set_bonus_score(H,V):-
    assert(temp_bool_H(_)),
    retractall(temp_bool_H(_)),
    assert(temp_bool_V(_)),
    retractall(temp_bool_V(_)),
    assert(temp_bool_V(V)),
    assert(temp_bool_H(H)).

set_bonus_score_H(H):-
    % assertz(temp_bool(_)),
    retractall(temp_bool_H(_)),
    assert(temp_bool_H(H)).

set_bonus_score_V(V):-
    % assertz(temp_bool(_)),
    retractall(temp_bool_V(_)),
    assert(temp_bool_V(V)).
% Dada una matriz M nos dice si la casilla (R,C) de la misma esta vacia y si es posible insertar alguna losa de algun color
% R -> Fila
% C -> Columna
% M -> Matriz
% Return
% !!! Uso del predicado dinamico dynamic_bool para saber si es posible insertar la ficha o no
% V -> Valor ubicado en la posicion (R,C) de la matriz M
valid_pos_to_insert_tile(R, C, M, V):-
    set_dynamic_bool_false,
    valid_pos(R,C),
    get_values([[R,C]], M, [V]),
    V =:= 0,
    set_dynamic_bool_true.

valid_pos_to_insert_tile(R, C, M, V).
% Devuelve True si se encuentra el color T en la fila R de la matriz W
% T -> Tile o Type o Color de la ficha
% R -> Fila a buscar
% W -> Matriz del jugador 
% Return
% Result, contiene el valor de la posicion 
color_in_row(T, R, W, Result):-
    %T1 is T-1,
    R1 is R-1,
    colors(T, T_str),
    find_col(T_str, R1, C),
    % R1 is R-1,
    % C1 is C-1,
    get_values([[R1,C]], W, [Result]),
    Result =:= 1.

% Dada una matriz M inserta en la posicion (R,C) la loseta de color Tile
% R -> Fila
% C -> Columna
% M -> Matriz
% V -> Valor ubicado en la posicion (R,C) de la matriz M
insert_tile(R, C, M, Tile,Mu):-
    update_mat_rc(M,R,C,Tile,Mu).
    % format("~n matriz Mu de insert_tile ~n"),
    % print(Mu).

%################################################# Calculando puntuaciones ############################################

% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos horizontalmente(fila) desde esa casilla
% moviendonos hacia la izquierda
% R -> Fila
% C -> Columna
% M -> Matriz
% Return:
% Void
calculate_row_score_left(R, C,  M):-
    CL is C - 1,    % Actualizamos el indice de la columna que se movera a la izqda(ColumnLeft)

    CL >= 0,        % Comparamos que se encuentre dentro de los limites de la matriz
    CL =< 4,
    get_values([[R,CL]], M, [V]),   % Buscando el valor de la casilla acutal de la matriz, asignamos el mismo a la variable V
    V =:= 1,                % Comprobamos que el valor de la casilla en la que estamos situados actualmente tenga 1(contiene una loza en la misma)

    set_bonus_score_H(1),
    retract(temp_score(B)), %    
    B1 is B+1,              % Actualizamos el valor de la cantidad de casillas contiguas que tenian alguna loza colocada
    assert(temp_score(B1)), %
    
    calculate_row_score_left(R, CL, M).
% Caso para cuando de fail o False no se detenga la ejecucion de la aplicacion.
calculate_row_score_left(R, C,  M).
% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos horizontalmente(fila) desde esa casilla
% moviendonos hacia la derecha
% R -> Fila
% C -> Columna
% M -> Matriz
% S -> Puntuacion
% Return:
% Void
calculate_row_score_right(R,C, M):-

    CR is C + 1,
    CR >= 0,
    CR =< 4,
    get_values([[R,CR]], M, [V]),
    V =:= 1,

    set_bonus_score_H(1),
    retract(temp_score(B)),
    B1 is B+1,
    assert(temp_score(B1)),
    
    calculate_row_score_right(R, CR, M).
% Caso para cuando de fail o False no se detenga la ejecucion de la aplicacion.
calculate_row_score_right(R,C, M).

% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos horizontalmente(fila) desde esa casilla
% moviendonos hacia ambos ladods(Izquierda y derecha)
% Args:
% R -> Fila
% C -> Columna
% M -> Matriz
% Return:
% Void
calculate_row_score(R,C, M):-
    calculate_row_score_left(R, C,  M),
    calculate_row_score_right(R, C,  M),
    !.
% Caso para cuando de fail o False no se detenga la ejecucion de la aplicacion.
calculate_row_score(R, C,  M).

% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos verticalmente(columna) desde esa casilla
% moviendonos hacia arriba
% R -> Fila
% C -> Columna
% M -> Matriz
% Return:
% Void
calculate_column_score_up(R,C,  M):-
    RU is R - 1,    % Actualizamos el indice de la fila que se movera hacia arriba(RowUp)

    RU >= 0,        % Comparamos que se encuentre dentro de los limites de la matriz
    RU =< 4,
    get_values([[RU,C]], M, [V]),   % Buscando el valor de la casilla acutal de la matriz, asignamos el mismo a la variable V
    V =:= 1,                % Comprobamos que el valor de la casilla en la que estamos situados actualmente tenga 1(contiene una loza en la misma)

    set_bonus_score_V(1),
    retract(temp_score(B)), %    
    B1 is B+1,              % Actualizamos el valor de la cantidad de casillas contiguas que tenian alguna loza colocada
    assert(temp_score(B1)), %

    calculate_column_score_up(RU,C,  M).
% Caso para cuando de fail o False no se detenga la ejecucion de la aplicacion.
calculate_column_score_up(R,C, M).

% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos verticalmente(columna) desde esa casilla
% moviendonos hacia abajo
% R -> Fila
% C -> Columna
% M -> Matriz
% Return:
% Void
calculate_column_score_down(R,C, M):-
    RD is R + 1,    % Actualizamos el indice de la fila que se movera hacia abajo(RowDown)

    RD >= 0,        % Comparamos que se encuentre dentro de los limites de la matriz
    RD =< 4,
    get_values([[RD,C]], M, [V]),   % Buscando el valor de la casilla acutal de la matriz, asignamos el mismo a la variable V
    V =:= 1,                % Comprobamos que el valor de la casilla en la que estamos situados actualmente tenga 1(contiene una loza en la misma)

    set_bonus_score_V(1),
    retract(temp_score(B)), %    
    B1 is B+1,              % Actualizamos el valor de la cantidad de casillas contiguas que tenian alguna loza colocada
    assert(temp_score(B1)), %

    calculate_column_score_down(RD,C, M).
% Caso para cuando de fail o False no se detenga la ejecucion de la aplicacion.
calculate_column_score_down(R,C, M).

% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos verticalmente(columna) desde esa casilla
% Args:
% R -> Fila
% C -> Columna
% M -> Matriz
% Return:
% Void
calculate_column_score(R,C, M):-
    calculate_column_score_up(R, C, M),
    calculate_column_score_down(R, C, M),
    !.
% Caso para cuando de fail o False no se detenga la ejecucion de la aplicacion.
calculate_column_score(R, C, M).


calculate_bonus_score:-
    temp_bool_H(H),
    temp_bool_V(V),
    H =:= 1,
    V =:= 1,
    retract(temp_score(B)), %    
    B1 is B+1,              % Actualizamos el valor de la cantidad de casillas contiguas que tenian alguna loza colocada
    assert(temp_score(B1)). %
calculate_bonus_score.
% Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos horizontal(fila) y verticalmente(columna) desde esa casilla
% R -> Fila
% C -> Columna
% M -> Matriz
% S -> Puntuacion
calculate_score(R,C, M, S):-
    set_temp_score(1),
    set_bonus_score(0,0),

    calculate_row_score(R, C,  M),
    calculate_column_score(R,C,  M),
    calculate_bonus_score,
    temp_score(S),
    !.

%################################################# Calculando puntuaciones ############################################

%################################################# Comprobando Filas, columnas y diagonales ############################################

dif_0(X):-
    X =\= 0.

cumplen_todos([],_).
cumplen_todos([X|Y],C):- T=..[C,X], T, cumplen_todos(Y,C).


row_is_filled(R1, W):-
    % colors(T, T_str),
    R is R1-1,
    set_dynamic_bool_false,
    get_row(R, W, Full_Row),
    % print(Full_Row),
    % findall(X, nth0(T, Full_Row, X), V),
    cumplen_todos(Full_Row, dif_0),
    set_dynamic_bool_true.
row_is_filled(R,W).


update_temp_score_using_dynamic_bool:-
    dynamic_bool(Bool),
    Bool =:= 1,
    retract(temp_score(B)), %    
    B1 is B+1,              % Actualizamos el valor de la cantidad de casillas contiguas que tenian alguna loza colocada
    assert(temp_score(B1)). %
update_temp_score_using_dynamic_bool.

calculate_rows_filled_amount_score(W, S):-
    set_temp_score(0),
    % Row 0
    row_is_filled(0, W),
    update_temp_score_using_dynamic_bool,
    % Row 1
    row_is_filled(1, W),
    update_temp_score_using_dynamic_bool,
    % Row 2
    row_is_filled(2, W),
    update_temp_score_using_dynamic_bool,
    % Row 3
    row_is_filled(3, W),
    update_temp_score_using_dynamic_bool,
    % Row 4
    row_is_filled(4, W),
    update_temp_score_using_dynamic_bool,
    temp_score(S),
    !.

col_is_filled(R, W):-
    % colors(T, T_str),
    set_dynamic_bool_false,
    get_row(R, W, Full_Row),
    % print(Full_Row),
    % findall(X, nth0(T, Full_Row, X), V),
    cumplen_todos(Full_Row, dif_0),
    set_dynamic_bool_true.
col_is_filled(R,W).

calculate_columns_filled_amount_score(W, S):-
    set_temp_score(0),
    % Row 0
    col_is_filled(0, W),
    update_temp_score_using_dynamic_bool,
    % Row 1
    col_is_filled(1, W),
    update_temp_score_using_dynamic_bool,
    % Row 2
    col_is_filled(2, W),
    update_temp_score_using_dynamic_bool,
    % Row 3
    col_is_filled(3, W),
    update_temp_score_using_dynamic_bool,
    % Row 4
    col_is_filled(4, W),
    update_temp_score_using_dynamic_bool,
    temp_score(S),
    !.

% dif_0_n_numbers((R,C)):-
%     get_values([[R,C]], W)

diag_is_filled(Tile, W):-
    set_dynamic_bool_false,
    colors(Tile, Tile_str),
    findall([R,C], find_col(Tile_str, R, C), L),
    % print(L),
    get_values(L, W, Full_Row),
    % print(Full_Row),

    cumplen_todos(Full_Row, dif_0),
    set_dynamic_bool_true.
diag_is_filled(Tile, W).

calculate_colors_filled_amount_score(W, S):-
    set_temp_score(0),
    % Row 0
    diag_is_filled(0, W),
    update_temp_score_using_dynamic_bool,
    % Row 1
    diag_is_filled(1, W),
    update_temp_score_using_dynamic_bool,
    % Row 2
    diag_is_filled(2, W),
    update_temp_score_using_dynamic_bool,
    % Row 3
    diag_is_filled(3, W),
    update_temp_score_using_dynamic_bool,
    % Row 4
    diag_is_filled(4, W),
    update_temp_score_using_dynamic_bool,
    temp_score(S),
    !.

calculate_row_col_diag_filled_score(W, S):-
    calculate_rows_filled_amount_score(W, S1),
    calculate_columns_filled_amount_score(W, S2),
    calculate_colors_filled_amount_score(W, S3),
    S is (S1*2) + (S2*7) + (S3*10).

%################################################# Comprobando Filas, columnas y diagonales ############################################


print_wall(Matrix):-
    nth0(0, Matrix, MR1),
    print(MR1),
    format("~n"),
    nth0(1, Matrix, MR2),
    print(MR2),
    format("~n"),
    nth0(2, Matrix, MR3),
    print(MR3),
    format("~n"),
    nth0(3, Matrix, MR4),
    print(MR4),
    format("~n"),
    nth0(4, Matrix, MR5),
    print(MR5),
    format("~n").
