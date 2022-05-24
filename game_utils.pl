%predicado de jugadas que tiene el numero de la fabrica, el color y la cantidad de fichas de ese color, la fabrica 0 representa el centro
:-dynamic plays/3.


%se encarga de buscar todas las combinaciones de jugadas con los azulejos en las fabricas y en el centro
search_plays():-.

%cuando se realiza una jugada se encarga de quitar esa jugada y enviar el resto de las fichas de esa fabrica al centro
update_plays(Factory_number,Color):-.

%al principio de cada ronda se encarga de crear todas las jugadas posibles de la ronda con los azulejos que hay en las fabricas
create_plays():-
    factory(Factories_number,Colors),
    append_colors_to_plays(Factories_number,Colors).

%va agregando cada color de una fabrica a las jugadas posibles
append_colors_to_plays(Factories_number,[]):-!.
append_colors_to_plays(Factories_number,[Color1|RColors]):-
    apppend_play(Factories_number,Color1),
    append_colors_to_plays(Factories_number,RColors).

%agrega una jugada cuando el color aun no estaba registrado
append_play(Factories_number,Color):-
    not(plays(Factories_number,Color,X)),
    !,
    assert(plays(Factories_number,Color,1)).
%agrega una jugada cuando el color ya estaba en la fabrica, o sea agrega 1 al contador
append_play(Factories_number,Color):-
    plays(Factories_number,Color,Count),
    retract(plays(Factories_number,Color,Count)),
    Count1 is Count+1,
    assert(plays(Factories_number,Color,Count1)).
