:-[game]



start_game(Players_number):-
    % append_colors(),
    % append_factories_per_player(),
    % create_bag(),
    % create_cementery(),
    factories_per_player(Players_number,Factories_number),
    inicialize(Players_number,Factories_number),
    create_players(Players_number),
    %create_factories(Players_number),
    start_round().




