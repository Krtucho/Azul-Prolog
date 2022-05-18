

take_n([X|Y],0,X):-!.
take_n([X|Y],N,R):-N>0, N1 is N-1, take_n(Y,N1,R).

eliminate_n(0,[X|Y],Y):-!.
eliminate_n(N,[X|Y],R):-N1 is N-1, eliminate_n(N1,Y,R).

len_list([], 0):-!.
len_list([X|L], R):- cantidad(L,F),R is F+1.



create_bag(Bag):-Bag=['negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'negro', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'azul', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'amarillo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'rojo', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco', 'blanco'].



create_players(Players_number,Players):-.



create_factories(2,Bag,Factories):-fill_the_factories(5,Bag,Factories).
create_factories(3,Bag,Factories):-fill_the_factories(7,Bag,Factories).
create_factories(4,Bag,Factories):-fill_the_factories(9,Bag,Factories).

fill_that_factory(0,Actual_Factory):-!.
fill_that_factory(Count,[X|Elements]):-
    Count1=Count-1,
    Len is len_list(X),
    random(0,Len,Random),
    take_n(Bag,Random,X),
    eliminate_n(Random,Bag,Bag),
    fill_that_factory(Count1,Elements).   


fill_the_factories(0,Bag,Factories):-!.
fill_the_factories(Number_of_Factories,Bag,[Actual_Factory|Factories]):-
    fill_that_factory(0,Actual_Factory),
    Number_of_Factories1=Number_of_Factories-1,
    fill_the_factories(Number_of_Factories1,Bag,Factories).



start_game(Players_number):-
    create_bag(Bag),
    create_players(Players_number,Players),
    create_factories(Players_number,Bag,Factories),
    start_round(Bag,Players,Factories).




