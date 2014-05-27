% 1 Idiot Sort
% Patrick Rengifo 09-10703
% Daniel Rodriguez 09-10735

% idiotsort(Lista,Ordenada) :- Ordenada tiene los elementos de 
%                              Lista ordenados ascendentemente
% idiotsort(list,list) (?,?)

idiotsort([],[])   :- !.
idiotsort([X],[X]) :- !.
idiotsort(X,Y)     :- permutation(X,Y), ordenada(Y).

ordenada([X]).
ordenada([X|[X1|Xs]]) :- X=<X1, ordenada([X1|Xs]).