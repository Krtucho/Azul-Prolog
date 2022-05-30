%                         ii    ll                                        ll    
%                 tt            ll                                        ll    
%                 tt            ll                                        ll    
%   uu      uu  tttttttt  ii    ll      ssssssss            pp  pppppp    ll    
%   uu      uu    tt      ii    ll      ss    ssss          pppp    pp    ll    
%   uu      uu    tt      ii    ll      ss                  pp      pp    ll    
%   uu      uu    tt      ii    ll      ssssss              pp      pp    ll    
%   uu      uu    tt      ii    ll            ssss          pp      pp    ll    
%   uu      uu    tt      ii    ll              ss          pp      pp    ll    
%   uu      uu    tt      ii    ll    ssss      ss          pp      pp    ll    
%     uuuuuuuu      tttt  ii    llllll  ssssssss      ..    pppppppp      llllll
%                                                           pp                  
%                                                           pp                  
%                                                           pp                  

% Booleano dinamico que se le podra asignar 0 = False o 1 = True
:- dynamic dynamic_bool/1.


start_dynamic_bool:-
    assert(dynamic_bool(1)).

set_dynamic_bool_true:-
    assert(dynamic_bool(_)),
    retractall(dynamic_bool(_)),
    assert(dynamic_bool(1)).
    
set_dynamic_bool_false:-
    assert(dynamic_bool(_)),
    retractall(dynamic_bool(_)),
    assert(dynamic_bool(0)).

concatList([], Z, Z).
concatList([A|X], Y, [A|Z]) :- 
    concatList(X, Y, Z).

%S: 
%[ 5   6 
%  3   4 ] 
% Represented in prolog as: S = [ [5,6], [3,4]].

%I'm trying to write a recursive function to get a cell value such that cell_values (Cells, Matrix, Values) would return a list of the values from a list of the cell.
%Example: cell_values ([[0,0], [0,1]], S, Values) --> Values = [5, 6]. Where S is the matrix above.
% R -> 1st value(position x,y) to find
% C -> 2nd value(position x,y) to find
% T -> nth value(position x,y) to find
% M -> Matrix
% V -> 1st value found
% VT -> nth value found
get_values([], _, []).
% get_values([[R, C]], M, [V]):-!,
%     nth0(R, M, Row),
%     nth0(C, Row, V).
get_values([[R, C]| T], M, [V|VT]) :-
    nth0(R, M, Row),
    nth0(C, Row, V),
    get_values(T, M, VT).

%?- update_mat_rc([[1,2,3],[4,5,6],[7,8,9]],1,1,x,M).
%M = [[1, 2, 3], [4, x, 6], [7, 8, 9]].
update_mat_rc(Mc,R,C,V,Mu) :-
    nth0(R,Mc,Rc,Mt),
    nth0(C,Rc,_,Rt),
    nth0(C,Ru,V,Rt),
    nth0(R,Mu,Ru,Mt).
    % format("~n matriz Mu ~n"),
    % print(Mu),
    % format("~n matriz Mt ~n"),
    % print(Mt).

% Index -> Posicion de la lista a buscar
% Matrix(Matriz o lista en la que vamos a buscar)
% Fila resultante
get_row(Index, Matrix, Row):-
    nth0(Index, Matrix, Row).

%?- get_col( [[1,2], [3, 4], [5,6]], 0, Col).
%Col = [1, 3, 5].

%?- get_col( [[1,2], [3, 4], [5,6]], I, Col).
%I = 0,
%Col = [1, 3, 5] ;
%I = 1,
%Col= [2, 4, 6].
get_col([], _, []).
get_col([X|Xs], I, [Y|Ys]) :-
    nth0(I, X, Y),
    get_col(Xs, I, Ys).

