% Patrick Rengifo 09-10703
% Daniela Rodríguez 09-10735
% Ejercicio 4

:- dynamic(visit/3).
drop(0, Y, Y).
drop(N, [Head|Tail], Y) :-
    M is N-1,
    drop(M, Tail, Y).

go(X, Y, I, P, Pnn) :-
    Xn is X*10,
    visit(Xn, Y, In),
    I > In,
    drop(In, P, Pnn), !.

go(X, Y, I, P, Pnn) :-
    Xn is X * 10,
    D is Xn // Y,
    M is Xn mod Y,
    append(P, [D], Pn),
    In is I + 1,
    asserta(visit(Xn,Y,I)),
    go(M, Y, In, Pn, Pnn).


periodo(X, Y, P) :-
    X >= 0, Y > 0,
    X >= Y,
    Xn is X mod Y,
    periodo(Xn, Y, P).

periodo(X, Y, P) :-
    X >= 0, Y > 0,
    go(X, Y, 0, [], P),
    retractall(visit(A,B,C)).
