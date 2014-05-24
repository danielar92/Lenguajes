% Patrick Rengifo 09-10703
% Daniela Rodríguez 09-10735
% Ejercicio 2


% Caso base donde una lista es vacía. La suma es 0
list_sum([], 0).

% Suma de los elementos en una lista. Triunfa si la suma es igual a TotalSum
list_sum([Head | Tail], TotalSum) :-
    list_sum(Tail, Sum1),
    TotalSum is Head + Sum1.

% Chequeamos que las sumas de las filas columnas y diagonales sean iguales
check(A, B, C, D) :-
    list_sum(A, SumA),
    list_sum(B, SumB),
    list_sum(C, SumC),
    list_sum(D, SumD),
    SumA == SumB,
    SumB == SumC,
    SumC == SumD.


% Lo unificamos con todas las permutaciones del cuadrado mágico 4x4
diabolico([A, B, C, D,
           E, F, G, H,
           I, J, K, L,
           M, N, O, P]) :-

    permutation([1,2,3,4,
                 5,6,7,8,
                 9,10,11,12,
                 13,14,15,16],
                [A, B, C, D,
                 E, F, G, H,
                 I, J, K, L,
                 M, N, O, P]),
    check([A, B, C, D],
          [E, F, G, H],
          [I, J, K, L],
          [M, N, O, P]),
    check([A, E, I, M],
          [B, F, J, N],
          [C, G, K, O],
          [D, H, L, P]),
    check([M, B, G, L],
          [I, N, C, H],
          [A, F, K, P],
          [E, J, O, D]),
    check([P, C, F, I],
          [L, O, B, E],
          [H, K, N, A],
          [D, G, J, M]),
    check([A, B, C, D],
          [A, E, I, M],
          [M, B, G, L],
          [P, C, F, I]).
