[game_utils].
[game].


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
    % print("entro a round   "),
    fill_factories(Factories_number),
    create_plays(Factories_number).
    % play_to_end_round(),
    % end_of_round(),
    % comprobate_end_game().



start_game(Players_number):-
    % append_colors(),
    % append_factories_per_player(),
    % create_bag(),
    % create_cementery(),
    % append_factories_per_player(),
    factories_per_player(Players_number,Factories_number),
    inicialize_game(Factories_number),
    
    % create_players(Players_number),
    %create_factories(Players_number),
    % print("voy a entrar a round    "),
    round(Players_number,Factories_number).
    % print("salgo de round    ").


