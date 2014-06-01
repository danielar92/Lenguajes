% 2 Diabolic
% Patrick Rengifo 09-10703
% Daniela Rodriguez 09-10735


:- dynamic(visit/1).

% Distintas operaciones sobre la matriz

transpose([A,B,C,D,
           E,F,G,H,
           I,J,K,L,
           M,N,O,P],
          [M,I,E,A,
           N,J,F,B,
           O,K,G,C,
           P,L,H,D]).

reflect([A,B,C,D,
         E,F,G,H,
         I,J,K,L,
         M,N,O,P],
        [P,O,N,M,
         L,K,J,I,
         H,G,F,E,
         D,C,B,A]).

rotate_rows([A,B,C,D,
             E,F,G,H,
             I,J,K,L,
             M,N,O,P],
            [M,N,O,P,
             A,B,C,D,
             E,F,G,H,
             I,J,K,L]).

rotate_cols([A,B,C,D,
             E,F,G,H,
             I,J,K,L,
             M,N,O,P],
            [D,A,B,C,
             H,E,F,G,
             L,I,J,K,
             P,M,N,O]).

convolution([A,B,C,D,
             E,F,G,H,
             I,J,K,L,
             M,N,O,P],
           [A,D,H,E,
            B,C,G,F,
            N,O,K,J,
            M,P,L,I]).


% Trans.....mcdito

initial([1,8,10,15,
         14,11,5,4,
         7,2,16,9,
         12,13,3,6]).

go(X) :- visit(X).


go(X) :-
    \+ visit(X),
    assertz(visit(X)),
    transpose(X, Y),
    go(Y),
    reflect(X, Z),
    go(Z),
    rotate_rows(X,W),
    go(W),
    rotate_cols(X,W1),
    go(W1),
    convolution(X,W2),
    go(W2).


diabolico(P) :-
    retractall(visit(A)),
    initial(X),
    go(X),
    visit(P).


stopwatch(Predicate) :-
    real_time(Start),
    call(Predicate),
    real_time(Finish),
    Elapsed is (Finish - Start) / 1000,
    format('~4f seg~N',[Elapsed]), !.
