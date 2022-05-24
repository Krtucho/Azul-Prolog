[game_utils].

inicialize_game(Factories_number):-
    % append_colors(),
    % append_factories_per_player(),
    create_bag(),
    create_cementery(),
    create_center(),
    create_factories(Factories_number).


%al principio de cada ronda se encarga de crear todas las jugadas posibles de la ronda con los azulejos que hay en las fabricas
create_plays(0):-!.
create_plays(Factories_number):-
    % print("entro").
    factory(Factories_number,Colors),
    Factories_number1 is Factories_number-1,
    % print(Factories_number),
    append_colors_to_plays(Factories_number,Colors),
    create_plays(Factories_number1).

%va agregando cada color de una fabrica a las jugadas posibles
append_colors_to_plays(Factories_number,[]):-!.
append_colors_to_plays(Factories_number,[Color1|RColors]):-
    % print("                 "),
    % print(Color1),
    append_play(Factories_number,Color1),
    append_colors_to_plays(Factories_number,RColors).

%agrega una jugada cuando el color aun no estaba registrado
append_play(Factories_number,Color):-
    not(plays(Factories_number,Color,X)),
    !,
    % print("voy a hacer assert"),
    assert(plays(Factories_number,Color,1)).
%agrega una jugada cuando el color ya estaba en la fabrica, o sea agrega 1 al contador
append_play(Factories_number,Color):-
    % print("entre aqui  "),
    % print(Factories_number),
    % print("    "),
    % print(Color),
    plays(Factories_number,Color,Count),
    retract(plays(Factories_number,Color,Count)),
    Count1 is Count+1,
    assert(plays(Factories_number,Color,Count1)).

%cuando se realiza una jugada se encarga de quitar esa jugada y enviar el resto de las fichas de esa fabrica al centro
% update_plays(Factory_number,Color):-.



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

