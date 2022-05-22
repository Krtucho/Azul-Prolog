% aqui esta representada la bolsa del juego con los pares(color, cantidad de azulejos de ese color)
:-dynamic bag/2.
% aqui estan representadas las factorias del juego por su numero y que tiene una lista con 4 azulejos
:-dynamic factory/2.
:-dynamic cementery/2.
:-dynamic colors/2.
:-dynamic center/2.
:-dynamic factories_per_player/2.


inicialize_game(Factories_number):-
    append_colors(),
    % append_factories_per_player(),
    create_bag(),
    create_cementery(),
    create_center(),
    create_factories(Factories_number).

% para guardar una relacion de numeros con los colores en juego
append_colors():-
    assert(colors(1,'negro')),
    assert(colors(2,'azul')),
    assert(colors(3,'amarillo')),
    assert(colors(4,'rojo')),
    assert(colors(5,'blanco')).

%cantidad de fabricas que se crean segun la cantidad de jugadores de la partida
append_factories_per_player():-
    assert(factories_per_player(2,5)),
    assert(factories_per_player(3,7)),
    assert(factories_per_player(4,9)).


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
    assert(factory(1,[0])).

create_n_factories(N):-
    N1 is N-1,
    assert(factory(N,[0])),
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
    % print(Factory_number),
    % print('                   '),
    Count1 is N-1,
    % print(Count1),
    % print('                   '),
    bag('total',Len),
    % print(Len),
    % print('                   '),
    random(1,Len,Random),
    % print(Random),
    % print('                   '),
    search_pos_n_on_bag(Random,0,1,Color),
    % print(Color),
    % print('                   '),
    colors(Color,ColorString),
    % print(ColorString),
    % print('                   '),
    append_tile_to_factory(Factory_number,ColorString),    
    rest_n_color_c_to_bag(1,ColorString),
    fill_that_factory(Factory_number,Count1).   


% primera seccion de la ronda  en donde se rellenan todas las factorias
fill_factories(1):-!,
    fill_that_factory(1,4).
fill_factories(Factories_number):-
    N1 is Factories_number-1,
    fill_that_factory(Factories_number,4),
    fill_factories(N1).


%segunda seccion de la ronda en donde todos los jugadores juegan hasta que se acaben todas las fichas de las fabricas y del centro
play_to_end_round():-.

%tercera seccion de la ronda en donde se colocan los azulejos de las escaleras de los jugadores en sus mosaicos y se suman las puntuaciones
end_of_round():-.

% cuarta seccion de la ronda en donde se realiza la comprobacion de que se cumplan las condiciones de finalizacion del juego
comprobate_end_game():-
    .


%desarrollo de una ronda

round(Players_number,Factories_number):-
    fill_factories(Factories_number).
    % play_to_end_round(),
    % end_of_round(),
    % comprobate_end_game().



start_game(Players_number):-
    % append_colors(),
    % append_factories_per_player(),
    % create_bag(),
    % create_cementery(),
    append_factories_per_player(),
    factories_per_player(Players_number,Factories_number),
    inicialize_game(Factories_number),
    % create_players(Players_number),
    %create_factories(Players_number),
    round(Players_number,Factories_number).