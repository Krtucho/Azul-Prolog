% aqui esta representada la bolsa del juego con los pares(color, cantidad de azulejos de ese color)
:-dynamic bag/2.
% aqui estan representadas las factorias del juego por su numero y que tiene una lista con 4 azulejos
:-dynamic factory/2.
:-dynamic cementery/2.
:-dynamic colors/2.
:-dynamic center/2.
:-dynamic factories_per_player/2.


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
search_pos_n_on_bag(N,Acumulated,ActualColor,Color):-Acumulated>=N,!,Color=ActualColor.
search_pos_n_on_bag(N,Acumulated,ActualColor,Color):-
    Acumulated<N,
    colors(ActualColor,ColorString),
    bag(ColorString,Count),
    AcumulatedNew=Acumulated+Count,
    ActualColor1=ActualColor+1,
    search_pos_n_on_bag(N,AcumulatedNew,ActualColor1,Color).


%crea las fabricas en dependencia de la cantidad de jugadores que participan
create_factories(Players_number):-           %definir como van a ser las fabricas
    factories_per_player(Players_number,Factories_number),
    retractall(factory(_,_)),
    fill_factories(Factories_number).

fill_that_factory(0):-!.
fill_that_factory(Factories_number):-
    Count1=Count-1,
    %bag('total',Len),
    random(1,5,Random),
    search_pos_n_on_bag(Random,0,1,Color),

    % take_n(Bag,Random,X),
    % eliminate_n(Random,Bag,Bag),
    fill_that_factory(Count1,Elements).   


% primera seccion de la ronda  en donde se rellenan todas las factorias
fill_factories(1):-fill_that_factory(1).
fill_factories(Factories_number):-
    N1 is Factories_number-1,
    fill_that_factory(Factories_number),
    fill_factories(N1).


%segunda seccion de la ronda en donde todos los jugadores juegan hasta que se acaben todas las fichas de las fabricas y del centro
play_to_end_round():-.

%tercera seccion de la ronda en donde se colocan los azulejos de las escaleras de los jugadores en sus mosaicos y se suman las puntuaciones
end_of_round():-.

% cuarta seccion de la ronda en donde se realiza la comprobacion de que se cumplan las condiciones de finalizacion del juego
comprobate_end_game():-.
