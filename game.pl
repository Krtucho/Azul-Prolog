:-[utils].
:-[game_utils].
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
search_pos_n_on_plays(0,Factory_number,Color,[(X,Y)|_]):-!,
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
    findall((Factory_number,Color),(plays(Factory_number,Color,Cant),not_total(Factory_number)),Factories),
    random(1,Total,Random),
    search_pos_n_on_plays(Random,Factory_number,Color,Factories).



% ###############################################-End Parte de Jugadas-########################################################################

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
    send_list_to_center(Plays_In_Factory).
%en esta lista en donde estan todas las unificaciones de las jugadas de una fabrica, se envian todas al centro
send_list_to_center([]).
send_list_to_center([(Factory_number,Color,Count)|Plays_In_Factory]):-    
    format("Quitando el color ~a con cantidad ~a ~n",[Color,Count]),
    % plays(10,'total',Total),
    % retract(plays(10,'total',Total)),
    retract(plays(Factory_number,Color,Count)),
    % Total1 is Total-1,
    % assert(plays(10,'total',Total1)),
    append_tiles_to_center(Color,Count),
    append_play(0,Color,Count),
    send_list_to_center(Plays_In_Factory).    


%cuando se realiza una jugada se encarga de quitar esa jugada y enviar el resto de las fichas de esa fabrica al centro
update_plays(Factory_number,Color):-
    % factory(Factory_number,_),
    retract(plays(Factory_number,Color,_)),    
    % retract(factory(Factory_number,_)),
    send_to_center(Factory_number),
    % print("Sali de Go to center"),
    retract(factory(Factory_number,_)),
    % factory('total',Total_old),
    % retract(factory('total',Total_Old)),
    % assert(factory('total',Total_New)),
    plays(10,'total',Total_Old),
    Total_New is Total_Old-1,
    retract(plays(10,'total',Total_Old)),
    assert(plays(10,'total',Total_New)),
    assert(factory(Factory_number,[])).

%calcula el valor que tendra la jugada, de esto depende que sea seleccionada, en el caso que complete una fila se suma dos al descarte y en caso contrario, este valor es el descarte
% calculate_play_value(0,Discard,Value):-!, Value is 2-Discard.
% calculate_play_value(_,Discard,Value):-Value is 0-Discard.
calculate_play_value(Discard,Value):-Discard >=0, !, Value is 2-Discard. 
calculate_play_value(Discard,Value):-Discard < 0,Value is 0.
% en caso de que se pueda poner el color en la fila se actualiza la mejor jugada hasta el momento
append_play_player(0,_,_,_,_):-!.
append_play_player(1,Value,Row,Actual_Player,Discard_Amount):-
    better_play_player(Actual_Player,_,_,Better_Actual_Value),
    Value >= Better_Actual_Value,
    retract(better_play_player(Actual_Player,_,_,Better_Actual_Value)),
    assert(better_play_player(Actual_Player,Row,Discard_Amount,Value)).
append_play_player(1,_,_,_,_).

%encargado de poner los azulejos en la escalera y enviar los que sobren al descarte en caso de la fila 6, van todos al descarte
put_tiles_in_row(Actual_Player,6,Color,Amount):-!.
put_tiles_in_row(Actual_Player,Row,Color,Amount):-
    update_row(Actual_Player, Color, Amount, Row).

%encargado de tanto actualizar la cantidad de descarte que tiene el jugador  como de mandar los azulejos al cementerio(simulacion del descarte)
drop_tiles_general(Actual_Player,Color,0):-!.
drop_tiles_general(Actual_Player,Color,Discard_Amount):-
    append_tiles_to_cementery(Color, Discard_Amount),
    drop_tiles(Actual_Player, Discard_Amount, Total).


%entre todas las filas en que un jugador puede poner un color selecciona la mas conveniente (la de mayor Value) y devuelve la cantidad de fichas que se descartan
choose_row_to_put_tiles(0,Actual_Player,_,_,Row,Discard_Amount):-!,
    better_play_player(Actual_Player,Row,Discard_Amount,_).
choose_row_to_put_tiles(Actual_Row,Actual_Player,Color,Amount,Row,Discard_Amount):-
    can_set_tiles_in_row(Actual_Player,Color, Amount, Actual_Row, NewA),% NewA es descartes y -espacios vacios
    % format("sali de can set tiles con newA ~a, Actual Row ~a ~n",[NewA,Actual_Row]),
    dynamic_bool(Can_set_in_that_row),
    format("se puede poner en la fila ~a ~a ~n",[Actual_Row,Can_set_in_that_row]),
    calculate_play_value(NewA,Value),
    Empty_Spaces is 0-NewA,
    append_play_player(Can_set_in_that_row,Value,Actual_Row,Actual_Player,Empty_Spaces),
    % better_play_player(Actual_Player, Better_Actual_Row, _, Better_Actual_Value),
    Actual_Row1 is Actual_Row-1,
    choose_row_to_put_tiles(Actual_Row1,Actual_Player,Color,Amount,Row,Discard_Amount).


%el jugador actual toma el color Color de la fabrica Factories_number y coloca los azulejos en su escalera
play(Actual_Player,Factories_number,Color):-
    plays(Factories_number,Color,Amount),
    update_plays(Factories_number,Color),
    % print("sali de update plays ~n"),
    Drop_value is 0-Amount,
    assert(better_play_player(Actual_Player,6,Amount,Drop_value)),%en caso de que no se pueda colocar en ninguna fila entonces se colocan todas las baldosas en el descarte
    choose_row_to_put_tiles(5,Actual_Player,Color,Amount,Row,Discard_Amount),
    format("El jugador ~a toma ~a fichas de color ~a de la fabrica ~a y las coloca en su fila ~a ~n",[Actual_Player,Amount,Color,Factories_number,Row]),
    put_tiles_in_row(Actual_Player,Row,Color,Amount),
    drop_tiles_general(Actual_Player,Color,Discard_Amount).


%comprueba si no quedan jugadas por tomar (plays) esta vacio,  devuelve 0 si se acabo la ronda devuelve un entero en otro caso
end_round(End):-
    plays(10,'total',Total),
    End=Total.

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
refill_bag():-
    cementery(CementeryColor,CementeryColorCount),
    bag(BagColor,BagColorCount),
    BagColor=:=CementeryColor,
    retract(cementery(CementeryColor, CementeryColorCount)),
    retract(bag(BagColor, BagColorCount)),
    assert(cementery(CementeryColor,0)),
    BagColorCountNew is BagColorCount+CementeryColorCount,
    assert(bag(BagColor,BagColorCountNew)).


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
fill_that_factory(Factory_number,0):-!.    
fill_that_factory(Factory_number,N):-
    Count1 is N-1,
    bag('total',Len),
    random(1,Len,Random),
    search_pos_n_on_bag(Random,0,1,Color),
    colors(Color,ColorString),
    append_tile_to_factory(Factory_number,ColorString),    
    rest_n_color_c_to_bag(1,ColorString),
    fill_that_factory(Factory_number,Count1).

%#############################################-End Crear cada parte necesaria en el juego-##############################################################

%##############################################-Parte de final de Ronda-######################################################################################






put_tile_in_wall():-
    % Dada una matriz M inserta en la posicion (R,C) la loseta de color Tile
    % R -> Fila
    % C -> Columna
    % M -> Matriz
    % V -> Valor ubicado en la posicion (R,C) de la matriz M
    insert_tile(R, C, M, Tile),
    % Dada una posicion (R,C) en la matriz M determina la puntuacion que se obtiene si nos movemos horizontal(fila) y verticalmente(columna) desde esa casilla
    % R -> Fila
    % C -> Columna
    % M -> Matriz
    % S -> Puntuacion
    calculate_score(R,C, M, S).

check_every_step(0,Players_number,Factories_number):-!.
check_every_step(Position,Players_number,Factories_number):-
    Position1 is Position-1,
    check_every_step(Position1,Players_number,Factories_number).



%##############################################-End Parte de final de Ronda-######################################################################################
