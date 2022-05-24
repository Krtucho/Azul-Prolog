:-[game].





















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

