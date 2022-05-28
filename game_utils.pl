% aqui esta representada la bolsa del juego con los pares(color, cantidad de azulejos de ese color)
:-dynamic bag/2.
% aqui estan representadas las factorias del juego por su numero y que tiene una lista con 4 azulejos
:-dynamic factory/2.
% aqui esta representado el cementerio del juego con los pares(color, cantidad de azulejos de ese color)
:-dynamic cementery/2.
% aqui esta representado el centro del juego con los pares(color, cantidad de azulejos de ese color)
:-dynamic center/2.
%predicado de jugadas que tiene el numero de la fabrica, el color y la cantidad de fichas de ese color, la fabrica 0 representa el centro
:-dynamic plays/3.
%se utiliza para guardar la mejor jugada que puede hacer un jugador en su turno, en este caso seria el primer argumento el jugador, en el segundo la fila
%en que se pondrian los azulejos previamente seleccionados como tercero la cantidad de azulejos que se descartan y 
%por ultimo el valor que tendria esta jugada
:-dynamic better_play_player/4.
% % %descarte a donde van las fichas que los jugadores no pueden poner en la escalera durante cada turno 
% % :-dynamic discard_player/2.


% :-dynamic factories_per_player/2.
% :-dynamic colors/2.
colors(1,'negro').
colors(2,'azul').
colors(3,'amarillo').
colors(4,'rojo').
colors(5,'blanco').

factories_per_player(2,5).
factories_per_player(3,7).
factories_per_player(4,9).

