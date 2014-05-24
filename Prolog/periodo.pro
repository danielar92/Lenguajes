% Patrick Rengifo 09-10703
% Daniela Rodríguez 09-10735
% Ejercicio 4

periodo(X,Y,Z) :- Z is (X/ Y).

my_digits(0,[]) :- !.
my_digits(N,[A|As]) :- N1 is floor(N/10), A is N mod 10, my_digits(N1, As).
