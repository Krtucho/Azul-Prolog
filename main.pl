% [game_utils].
[game].


% primera seccion de la ronda  en donde se rellenan todas las factorias
fill_factories(1):-!,
    fill_that_factory(1,4).
fill_factories(Factories_number):-
    N1 is Factories_number-1,
    fill_that_factory(Factories_number,4),
    fill_factories(N1).


%segunda seccion de la ronda en donde todos los jugadores juegan hasta que se acaben todas las fichas de las fabricas y del centro
%se le pasa 0 si se acabaron las jugadas posibles y por tanto se acabo la ronda y N>0 significa que no ha acabado
%se le pasa el jugador actual 
play_to_end_round(0,Actual_Player,Players_number):-!.
play_to_end_round(N,Actual_Player,Players_number):-
    choose_play(Factories_number,Color),
    update_plays(Factories_number,Color),
    play(Actual_Player,Factories_number,Color),
    Actual_Player1 is  Actual_Player+1,
    Actual_Player_mod is Actual_Player1 mod Players_number,
    Actual_Player_mod1 is Actual_Player_mod+1,
    end_round(End),
    play_to_end_round(End,Actual_Player_mod1).



%tercera seccion de la ronda en donde se colocan los azulejos de las escaleras de los jugadores en sus mosaicos y se suman las puntuaciones
% end_of_round():-.

% cuarta seccion de la ronda en donde se realiza la comprobacion de que se cumplan las condiciones de finalizacion del juego
% comprobate_end_game():-
%     .

%desarrollo de una ronda 
round(Players_number,Factories_number):-
    % print("entro a round   "),
    fill_factories(Factories_number),
    create_plays(Factories_number).
    % assert(plays(10,'total',0)),

    %aqui buscar el primer jugador de esta ronda


    %play_to_end_round(1,Actual_Player,Players_number).
    % end_of_round(),
    % comprobate_end_game().



start_game(Players_number):-
    factories_per_player(Players_number,Factories_number),
    inicialize_game(Factories_number),
    round(Players_number,Factories_number).


start:-
    start_game(2).

start.