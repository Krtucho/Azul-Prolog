% [game_utils].
:-[game].
:-[player].



%deja vacios todos los predicados dinamicos para que se pueda jugar el juego desde el comienzo
clear_all:-
    retractall(bag(_,_)),
    retractall(factory(_,_)),
    retractall(cementery(_,_)),
    retractall(center(_,_)),
    retractall(plays(_,_,_)),
    retractall(better_play_player(_,_,_,_)),
    retractall(first_player(_)),
    retractall(players(_,_,_,_,_,_,_,_,_)),
    retractall(temp_score(_)),
    retractall(dynamic_bool(_)).

% primera seccion de la ronda  en donde se rellenan todas las factorias
fill_factories(1):-!,
    fill_that_factory(1,4).
fill_factories(Factories_number):-
    N1 is Factories_number-1,
    fill_that_factory(Factories_number,4),
    fill_factories(N1).

%comprueba si el actual es el primer jugador en tomar algo del centro para que sea el primer jugador de la proxima ronda
comprobate_first_player(0,Actual_Player):-
    not(first_player(F)),
    assert(first_player(Actual_Player)),
    drop_tiles(Actual_Player, 1, _).    
comprobate_first_player(_,_).

%segunda seccion de la ronda en donde todos los jugadores juegan hasta que se acaben todas las fichas de las fabricas y del centro
%se le pasa 0 si se acabaron las jugadas posibles y por tanto se acabo la ronda y N>0 significa que no ha acabado
%se le pasa el jugador actual 
play_to_end_round(0,Actual_Player,Players_number):-!.
play_to_end_round(N,Actual_Player,Players_number):-
    choose_play(Factories_number,Color),
    % colors(Color,Color_String),
    comprobate_first_player(Factories_number,Actual_Player),    
    % update_plays(Factories_number,Color),
    play(Actual_Player,Factories_number,Color),
    %Actual_Player1 is Actual_Player+1,
    Actual_Player_mod is Actual_Player mod Players_number,
    Actual_Player_mod1 is Actual_Player_mod+1,
    end_round(End),
    play_to_end_round(End,Actual_Player_mod1,Players_number).



%tercera seccion de la ronda en donde se colocan los azulejos de las escaleras de los jugadores en sus mosaicos y se suman las puntuaciones
%va por cada jugador y al final comprueba que hayan suficientes azulejos en la bolsa, de no haberlos pasa los del cementerio a la bolsa y de no alcanzar
%se da por terminado el juego
end_of_round(0,Factories_number):-!.
end_of_round(Players_number,Factories_number):-
    check_every_row(5,Players_number,Factories_number),
    Players_number1 is Players_number-1,
    % Dada una matriz M inserta en la posicion (R,C) la loseta de color Tile
    % R -> Fila
    % C -> Columna
    % M -> Matriz
    % V -> Valor ubicado en la posicion (R,C) de la matriz M
    insert_tile(R, C, M, Tile),
    end_of_round(Players_number1,Factories_number).



% cuarta seccion de la ronda en donde se realiza la comprobacion de que se cumplan las condiciones de finalizacion del juego
comprobate_end_game(Players_number,Factories_number):-
    player_fill_row(Players_number,End_player),
    tiles_insufficient(Factories_number,End_tiles),
    End_Game is End_player +  End_tiles,
    end_game(End_Game,Players_number).



%desarrollo de una ronda 
round(Players_number,Factories_number):-
    % print("entro a round   "),
    first_player(Actual_Player),
    retract(first_player(Actual_Player)),
    fill_factories(Factories_number),
    create_players(Players_number),
    create_plays(Factories_number),
    %aqui buscar el primer jugador de esta ronda

    play_to_end_round(1,Actual_Player,Players_number).

    % end_of_round(Players_number,Factories_number).
    % comprobate_end_game(Players_number).



start_game(Players_number):-
    factories_per_player(Players_number,Factories_number),
    inicialize_game(Factories_number),
    assert(first_player(1)),
    round(Players_number,Factories_number).



start:-
    % start_game(2).
    factories_per_player(Players_number,Factories_number),
    inicialize_game(Factories_number),
    assert(first_player(1)),

    first_player(Actual_Player),
    retract(first_player(Actual_Player)),
    fill_factories(Factories_number),
    create_players(Players_number),
    create_plays(Factories_number),

    play_to_end_round(1,Actual_Player,Players_number).




start.