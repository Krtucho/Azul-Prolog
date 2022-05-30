% :-[utils].
% :-[game_utils].
:-[player].


inicialize_game(Factories_number):-
    % append_colors(),
    % append_factories_per_player(),
    % create_plays(Factories_number).
    assert(plays(10,'total',0)),
    create_bag(),
    create_cementery(),
    create_center(),
    create_factories(Factories_number).


% % %se crea el descarte que le corresponde a cada jugador, al comienzo es una lista vacia
% % create_discard(0):-!.
% % create_discard(Players_number):-
% %     assert(discard_player(Players_number,[])),
% %     Players_number1 is Players_number-1,
% %     create_discard(Players_number1).


%###########################################################-Parte de Jugadas-#########################################################

%al principio de cada ronda se encarga de crear todas las jugadas posibles de la ronda con los azulejos que hay en las fabricas
create_plays(0):-!.
create_plays(Factories_number):-
    factory(Factories_number,Colors),
    Factories_number1 is Factories_number-1,
    append_colors_to_plays(Factories_number,Colors),
    create_plays(Factories_number1).

%va agregando cada color de una fabrica a las jugadas posibles
append_colors_to_plays(Factories_number,[]):-!.
append_colors_to_plays(Factories_number,[Color1|RColors]):-
    % format("~n en la fabrica ~a encontro color ~a ~n",[Factories_number,Color1]),
    append_play(Factories_number,Color1,1),
    append_colors_to_plays(Factories_number,RColors).

%agrega una jugada cuando el color aun no estaba registrado en esa fabrica
append_play(Factories_number,Color,Count):-
    not(plays(Factories_number,Color,_)),
    !,
    plays(10,'total',Total),
    Total1 is Total+1,
    retract(plays(10,'total',Total)),
    assert(plays(Factories_number,Color,Count)),
    % format("crear la jugada fabrica: ~a con color: ~a y cant nueva ~a ~n",[Factories_number,Color,1]),
    % format("el total ahora es ~a ~n",[Total1]),
    assert(plays(10,'total',Total1)).
%agrega una jugada cuando el color ya estaba en la fabrica, o sea agrega la cantidad nueva al contador
append_play(Factories_number,Color,Count):-
    plays(Factories_number,Color,X),
    retract(plays(Factories_number,Color,X)),
    Count1 is Count+X,    
    % format("en la fabrica ~a se encontro el color ~a por vez ~a ~n",[Factories_number,Color,Count1]),
    assert(plays(Factories_number,Color,Count1)).

%se le pasa la lista para encontrar el elemento N
search_pos_n_on_plays(1,Factory_number,Color,[(X,Y)|_]):-!,
    % format("~n el seleccionado es ~a ~a ~n",[X,Y]),
    Color = Y,
    Factory_number = X.
search_pos_n_on_plays(N,Factory_number,Color,[(_,_)|Factories]):-
    % format("el par es (~a,~a)~n",[X,Y]),
    N1 is N-1,
    search_pos_n_on_plays(N1,Factory_number,Color,Factories).


%indica si es el play destinado a almacenar el total de las jugadas
not_total(N):- not(N=:=10).
%de todas las jugadas posibles, el jugador escoge una random (o sea devuelve el numero de la fabrica y el color)
choose_play(Factory_number,Color):-    
    plays(10,'total',Total),
    % format("Total:~a ~n",[Total]),
    findall((Factory_number,Color),(plays(Factory_number,Color,_),not_total(Factory_number)),Factories),
    % format("~n"),
    % print(Factories),
    Total1 is Total+1,
    random(1,Total1,Random),
    % format("~n Random:~a ~n",[Random]),
    search_pos_n_on_plays(Random,Factory_number,Color,Factories).
    % format("Se selecciono la jugada ~a, ~a ~n",[Factory_number,Color]).


% ###############################################-End Parte de Jugadas-########################################################################


%##############################################-Parte Esencial del Juego-########################################################################

%envia los azulejos al cementerio 
append_tiles_to_cementery(Color, Amount):-
    cementery(Color,Old_Amount),
    retract(cementery(Color,Old_Amount)),
    New_Amount is Old_Amount+Amount,
    assert(cementery(Color,New_Amount)).

%coloca Count azulejos de color Color en el Centro del Juego
append_tiles_to_center(Color,Count):-
    center(Color,Count_Old),
    center('total',Total_Old),
    Total_New is Count+Total_Old,
    retract(center('total',Total_Old)),
    retract(center(Color,Count_Old)),
    Count_New is Count+Count_Old,
    assert(center('total',Total_New)),
    assert(center(Color,Count_New)).


%cuando se toman azulejos de una fabrica se tiene que enviar el resto que queda en la fabrica al centro (factory 0)
send_to_center(Factory_number):-
    % plays(Factory_number,Color,Count),       
    findall((Factory_number,Color,Count), plays(Factory_number,Color,Count), Plays_In_Factory),
    % format("Enviadas al centro: ",[]),
    % print(Plays_In_Factory),
    send_list_to_center(Plays_In_Factory).

%en esta lista en donde estan todas las unificaciones de las jugadas de una fabrica, se envian todas al centro
send_list_to_center([]).
send_list_to_center([(Factory_number,Color,Count)|Plays_In_Factory]):-    
    % format("Quitando el color ~a con cantidad ~a ~n",[Color,Count]),
    % plays(10,'total',Total),
    % retract(plays(10,'total',Total)),
    % format("~n Enviando al centro ~a ~a ~a ~n",[Factory_number,Color,Count]),
    retract(plays(Factory_number,Color,Count)),
    % factory('total',Total_Old),
    % Total_New is Total_Old-Count,
    % retract(factory('total',_)),
    % assert(factory('total',Total_New)),
    plays(10,'total',Plays_Total_Old),
    retract(plays(10,'total',Plays_Total_Old)),
    Plays_Total_New is Plays_Total_Old-1,
    assert(plays(10,'total',Plays_Total_New)),
    % Total1 is Total-1,
    % assert(plays(10,'total',Total1)),
    append_tiles_to_center(Color,Count),
    append_play(0,Color,Count),
    send_list_to_center(Plays_In_Factory).    


%cuando se realiza una jugada se encarga de quitar esa jugada y enviar el resto de las fichas de esa fabrica al centro
%si es la fabrica 0 es el centro por lo que no hay que enviar las demas jugadas al centro
update_plays(0,Color):- !,
    plays(10,'total',Total_Old),
    Total_New is Total_Old-1,    
    retract(plays(10,'total',Total_Old)),
    assert(plays(10,'total',Total_New)),
    retract(plays(0,Color,_)).   
%cuando se realiza una jugada se encarga de quitar esa jugada y enviar el resto de las fichas de esa fabrica al centro
update_plays(Factory_number,Color):-
    % factory(Factory_number,_),
    plays(10,'total',Total_Old),
    Total_New is Total_Old-1,    
    retract(plays(10,'total',Total_Old)),
    assert(plays(10,'total',Total_New)),
    retract(plays(Factory_number,Color,_)),    
    % retract(factory(Factory_number,_)),
    send_to_center(Factory_number),
    % print("Sali de Go to center"),
    % factory('total',Total_old),
    % retract(factory('total',Total_Old)),
    % assert(factory('total',Total_New)),
    retract(factory(Factory_number,_)),
    assert(factory(Factory_number,[])).

%calcula el valor que tendra la jugada, de esto depende que sea seleccionada, en el caso que complete una fila se suma dos al descarte y en caso contrario, este valor es el descarte
% calculate_play_value(0,Discard,Value):-!, Value is 2-Discard.
% calculate_play_value(_,Discard,Value):-Value is 0-Discard.
calculate_play_value(Discard,Value):-Discard >= 0, !, Value is 2-Discard. 
calculate_play_value(Discard,Value):-Discard < 0,Value is 0.
% en caso de que se pueda poner el color en la fila se actualiza la mejor jugada hasta el momento
append_play_player(0,_,_,_,_):-!.
append_play_player(1,Value,Row,Actual_Player,Discard_Amount):-
    better_play_player(Actual_Player,_,_,Better_Actual_Value),
    Value > Better_Actual_Value,
    retract(better_play_player(Actual_Player,_,_,Better_Actual_Value)),
    assert(better_play_player(Actual_Player,Row,Discard_Amount,Value)).
append_play_player(1,_,_,_,_).


%llama update_row con la cantidad correcta de azulejos a poner en la fila
call_update_row(Difference,Row,Actual_Player,New_Amount, Color):- 
    Difference < 0,
    !,
    update_row(Actual_Player, Color, Row, Row).
call_update_row(Difference,Row,Actual_Player,New_Amount, Color):- 
    Difference >=0,
    !,
    update_row(Actual_Player, Color, New_Amount, Row).

%encargado de poner los azulejos en la escalera, en caso de la fila 6, van todos al descarte
put_tiles_in_row(Actual_Player,6,Color,Amount):-!.
put_tiles_in_row(Actual_Player,Row,Color,Amount):-
    get_row_n(Actual_Player,Row,(_,Old_Amount)),
    New_Amount is Old_Amount+Amount,
    Difference is Row-New_Amount,
    % update_row(Actual_Player, Color, Difference, Row).
    call_update_row(Difference,Row,Actual_Player,New_Amount, Color).


%printea el cementerio
print_cementery(0):-!.
print_cementery(N):-
    colors(N, N_str),
    cementery(N_str, A),

    format("Color ~a Cantidad: ~a ~n", [N_str, A]),
    N1 is N-1,
    print_cementery(N1).

%encargado tanto de actualizar la cantidad de descarte que tiene el jugador, como de mandar los azulejos al cementerio(simulacion del descarte)
drop_tiles_general(Actual_Player,Color,Discard_Amount):-Discard_Amount=<0,!.
drop_tiles_general(Actual_Player,Color,Discard_Amount):-
    colors(Color,Color_String),
    % format("El jugador ~a va a descartar ~a fichas de color ~a",[Actual_Player,Discard_Amount,Color_String]),
    append_tiles_to_cementery(Color_String, Discard_Amount),
    players(Actual_Player,_,_,_,_,_,_,_,Amount_Old),
    Total is Amount_Old + Discard_Amount,
    update_dropped_tiles(Actual_Player, Total).
    % format("~n Cementerio ~n"),
    % print_cementery(5).
   
    
    % drop_tiles(Actual_Player, Discard_Amount, Total).


%si es una posicion viable dondde poner fichas la agrega a las posiciones y calcula el valor de la jugada
put_play_player(0,_,_,_,_,_).
put_play_player(1,Actual_Player,Color, Amount, Actual_Row, NewA):-
    calculate_play_value(NewA,Value),
    % format("se puede poner en la fila ~a con descarte ~a y valor ~a ~n",[Actual_Row,NewA,Value]),

    % Empty_Spaces is 0-NewA,
    append_play_player(1,Value,Actual_Row,Actual_Player,NewA).

    % better_play_player(Actual_Player, Better_Actual_Row, _, Better_Actual_Value),

%entre todas las filas en que un jugador puede poner un color selecciona la mas conveniente (la de mayor Value) y devuelve la cantidad de fichas que se descartan
choose_row_to_put_tiles(0,Actual_Player,_,_,Row,Discard_Amount):-!,
    better_play_player(Actual_Player,Row,Discard_Amount,_),
    retract(better_play_player(Actual_Player,Row,Discard_Amount,_)).
choose_row_to_put_tiles(Actual_Row,Actual_Player,Color,Amount,Row,Discard_Amount):-
    can_set_tiles_in_row(Actual_Player,Color, Amount, Actual_Row, NewA),% NewA es descartes y -espacios vacios
    % format("sali de can set tiles con newA ~a, Actual Row ~a ~n",[NewA,Actual_Row]),
    dynamic_bool(Can_set_in_that_row),
    put_play_player(Can_set_in_that_row,Actual_Player,Color, Amount, Actual_Row, NewA),
    Actual_Row1 is Actual_Row-1,    
    choose_row_to_put_tiles(Actual_Row1,Actual_Player,Color,Amount,Row,Discard_Amount).


%el jugador actual toma el color Color de la fabrica Factories_number y coloca los azulejos en su escalera
play(Actual_Player,Factories_number,Color):-
    colors(Color,Color_String),
    plays(Factories_number,Color_String,Amount),
    update_plays(Factories_number,Color_String),
    % print("sali de update plays ~n"),
    Drop_value is 0-Amount,
    assert(better_play_player(Actual_Player,6,Amount,Drop_value)),%en caso de que no se pueda colocar en ninguna fila entonces se colocan todas las baldosas en el descarte
    choose_row_to_put_tiles(5,Actual_Player,Color,Amount,Row,Discard_Amount),    
    format("El jugador ~a toma ~a azulejos de color ~a de la fabrica ~a, las coloca en su fila ~a y descarta ~a azulejos. ~n",[Actual_Player,Amount,Color_String,Factories_number,Row,Discard_Amount]),
    put_tiles_in_row(Actual_Player,Row,Color,Amount),
    drop_tiles_general(Actual_Player,Color,Discard_Amount).


%comprueba si no quedan jugadas por tomar (plays) esta vacio,  devuelve 0 si se acabo la ronda devuelve un entero en otro caso
end_round(End):-
    plays(10,'total',Total),
    End=Total.
%##############################################-End Parte Esencial del Juego-########################################################################

%#############################################-Crear cada parte necesaria en el juego-###############################################################
% lugar donde se van las fichas al ser descartadas de los tableros de los jugadores
create_cementery():-
    retractall(cementery(_,_)),
    assert(cementery('negro', 0)),
    assert(cementery('azul', 0)),
    assert(cementery('amarillo', 0)),
    assert(cementery('rojo', 0)),
    assert(cementery('blanco', 0)),
    assert(cementery('total',0)).

% centro del juego, donde van los azulejos cuando se sacan de las fabricas
create_center():-
    retractall(center(_,_)),
    assert(center('negro', 0)),
    assert(center('azul', 0)),
    assert(center('amarillo', 0)),
    assert(center('rojo', 0)),
    assert(center('blanco', 0)),
    assert(center('total',0)).


% rellena la bolsa cuando se acaban las fichas en esta
refill_bag():-refill_bag_per_color(5).
% rellena la bolsa por cada uno de los colores
refill_bag_per_color(0):-!.
refill_bag_per_color(ColorInt):-
    colors(ColorInt,Color),
    cementery(Color,CementeryColorCount),
    bag(Color,BagColorCount),
    % BagColor=:=CementeryColor,
    retract(cementery(Color, CementeryColorCount)),
    retract(bag(Color, BagColorCount)),
    assert(cementery(Color,0)),
    BagColorCountNew is BagColorCount+CementeryColorCount,
    bag('total',Total),
    retract(bag('total',Total)),
    NewTotal is Total+CementeryColorCount,
    assert(bag('total',NewTotal)),
    assert(bag(Color,BagColorCountNew)),
    Color_Int_New is ColorInt-1,
    refill_bag_per_color(Color_Int_New).


% la bolsa es de la forma (color,cantidad de azulejos de ese color que tiene) y tiene un campoo en el que dice la cantidad total de azulejos que hay en la bolsa(total)
create_bag():-
    %retractall(bag(_,_,)),
    assert(bag('negro', 20)),
    assert(bag('azul', 20)),
    assert(bag('amarillo', 20)),
    assert(bag('rojo', 20)),
    assert(bag('blanco', 20)),
    assert(bag('total',100)).



%busca la ficha que ocupa la posicion n en la bolsa
search_pos_n_on_bag(N,Acumulated,ActualColor,Color):-
    Acumulated>=N,
    !,
    % write('caso base'),
    % write('~n'),
    % write(N~n),
    Color is ActualColor-1.
search_pos_n_on_bag(N,Acumulated,ActualColor,Color):-
    Acumulated<N,
    % write(N),
    % write('~n'),
    % write(ActualColor),
    % write('~n'),
    colors(ActualColor,ColorString),
    % write(ColorString),
    % write('~n'),
    % write(Acumulated),
    % write('~n'),
    bag(ColorString,Count),
    AcumulatedNew is Acumulated+Count,
    ActualColor1 is ActualColor+1,
    search_pos_n_on_bag(N,AcumulatedNew,ActualColor1,Color).


%agrega el azulejo seleccionado a la lista de azulejos de la fabrica de numero Factory
append_tile_to_factory(Factory,Color):-
    factory(Factory,Tiles),
    TilesNew=[Color|Tiles],
    retract(factory(Factory,Tiles)),
    assert(factory(Factory,TilesNew)).



%crea las fabricas vacias
create_n_factories(1):-
    !,
    assert(factory(1,[])).

create_n_factories(N):-
    N1 is N-1,
    assert(factory(N,[])),
    create_n_factories(N1).


%manda a crear las fabricas en dependencia de la cantidad de jugadores que participan
create_factories(Factories_number):-           
    % factories_per_player(Players_number,Factories_number),
    %assert(factory(1,[0])),
    %retractall(factory(_,_)),
    create_n_factories(Factories_number).
    %fill_factories(Factories_number).

% saca de la bolsa N azulejos de color C
rest_n_color_c_to_bag(N,C):-
    bag(C,Count),
    Count1 is Count-N,
    retract(bag(C,Count)),
    assert(bag(C,Count1)),
    bag('total',TotalOld),
    TotalNew is TotalOld-N,
    retract(bag('total',TotalOld)),
    assert(bag('total',TotalNew)).



%pone 4 azulejos en la fabrica de numero factory_number
fill_that_factory(Factory_number,0):-!,
    factory(Factory_number,Tiles),
    format("~n Fabrica ~a: ",[Factory_number]),
    print(Tiles).    
fill_that_factory(Factory_number,N):-
    Count1 is N-1,
    bag('total',Len),
    Len1 is Len + 1,
    random(1,Len1,Random),
    search_pos_n_on_bag(Random,0,1,Color),
    colors(Color,ColorString),
    append_tile_to_factory(Factory_number,ColorString),    
    rest_n_color_c_to_bag(1,ColorString),
    fill_that_factory(Factory_number,Count1).

%#############################################-End Crear cada parte necesaria en el juego-##############################################################

%##############################################-Parte de final de Ronda-######################################################################################

%vacia el escalon n de la escalera de player y manda n-1 azulejos al cementerio
empty_n_row_of_player(Player,Row):-
    get_row_n(P, Row, Result),    
    update_row(Player, 0, 0, Row),
    Extra is Row-1,
    append_tiles_to_cementery(Color, Extra).

% si esta llena una fila de la esscalera, encargado de colocar un azulejo en el muro de un jugador despues de que haya completado una fila de ese color
% put_tile_in_wall(0,_,_,_):-!.
put_tile_in_wall(0,Player,Row,Color):-
    players(Player,S,R1,R2,R3,R4,R5,Matrix,D),
    Row_Matrix is Row-1,
    colors(Color,Color_String),    

    find_col(Color_String,Row_Matrix,Column),
    insert_tile(Row_Matrix, Column, Matrix, 1,New_Matrix),

    retract(players(Player,S,R1,R2,R3,R4,R5,Matrix,D)),
    assert(players(Player,S,R1,R2,R3,R4,R5,New_Matrix,D)),
    format("Jugador: ~a Fila: ~a Columna: ~a ~n", [Player, Row_Matrix, Column]),
    calculate_score(Row_Matrix,Column, Matrix, Score_to_Add),
    print("Matrix"),
    format("~n"),
    print_wall(New_Matrix),
    format("Puntuacion a annadir: ~a ~n", [Score_to_Add]),
    empty_n_row_of_player(Player,Row),
    add_score(Player, Score_to_Add).
put_tile_in_wall(N,_,_,_).

%chequea en cada escalon de la escalera si se ha completado y lo manda entonces a 
check_every_row(0,Actual_Player):-!.
check_every_row(Position,Actual_Player):-
    get_row_n(Actual_Player,Position,(Color,Amount)),
    Full is Position-Amount,
    put_tile_in_wall(Full,Actual_Player,Position,Color),
    Position1 is Position-1,
    check_every_row(Position1,Actual_Player).



%##############################################-End Parte de final de Ronda-#####################################################################################

%###############################################-Parte del final del juego-#####################################################################


%comprueba si el jugador completo alguna fila 
player_filled(0,Players_number,End):-!,
    Players_number1 is Players_number-1,
    player_fill_row(Players_number1,End).
player_filled(N,Players_number,End):-End is 1.


%encargado de comprobar si alguno de los jugadores completo una fila para terminar el juego
player_fill_row(0,End):-!, End is 0.
player_fill_row(Players_number,End):-
    players(Players_number,_,_,_,_,_,_,Matrix,_),
    row_is_filled(1, Matrix),
    dynamic_bool(B1),
    row_is_filled(2, Matrix),
    dynamic_bool(B2),
    row_is_filled(3, Matrix),
    dynamic_bool(B3),
    row_is_filled(4, Matrix),
    dynamic_bool(B4),
    row_is_filled(5, Matrix),
    dynamic_bool(B5),
    Player_filled is B1 + B2 + B3 + B4 + B5,%es 0 si ninguna fila se ha llenad0
    player_filled(Player_filled,Players_number,End).


%si la cantidad de fichas que se requieren para rellenar las fabricas es menor que la cantidad que hay actualmente en la bolsa, esta se rellena con las fichas del cementerio
refill_bag_from_cementery(N):- N >= 0,!.
refill_bag_from_cementery(N):- 
    cementery('total',Total),
    format("Se van a pasar ~a azulejos del cementerio a la bolsa ~n",[Total]),
    refill_bag().

%devuelve 1 si no quedan suficientes fichas para rellenar las fabricas y 0 en caso contrario
sufficient_tiles(New_Need_refill, End):- 
    New_Need_refill < 0,
    !,
    format("Quedan menos azulejos de los que se necesitan para rellenar las fabricas, por tanto el juego ha acabado. ~n"),
    End is 1.
sufficient_tiles(New_Need_refill, End):- End is 0.

%encargado de revisar si en la bolsa no quedan suficientes azulejos, si es el caso pasa los del cementerio a la bolsa y si siguen siendo insuficientes
%el juego se da por terminado (end es 1 si no alcanzan los azulejos)
tiles_insufficient(Factories_number,End):-
    bag('total',Total),
    Tiles_need is Factories_number * 4,
    Need_refill is Total - Tiles_need,
    refill_bag_from_cementery(Need_refill),    
    bag('total',New_Total),
    New_Need_refill is New_Total - Tiles_need,
    sufficient_tiles(New_Need_refill, End).


%por cada uno de los jugadores busca la cantidad de diagonales, filas y columnas completadas para sumar a su puntuacion 
actualize_score_end_game_per_player(0):-!.
actualize_score_end_game_per_player(Player):-
    players(Player,_,_,_,_,_,_,Matrix,_),
    calculate_row_col_diag_filled_score(Matrix, Gained_Score),
    add_score(Player, Gained_Score),
    Player1 is Player-1,
    actualize_score_end_game_per_player(Player1).


%se llama con 0 o 1 como primer argumento donde 1 significa que el juego ha acabado y 0 que no
%se encarga de mandar a contar la cantidad de diagonales, filas y columnas completadas para sumar a la puntuacion de cada jugador
end_game(0,_):-!.
end_game(N,Players_number):-
    actualize_score_end_game_per_player(Players_number).

%###############################################-End Parte del final del juego-#####################################################################














