% Definicion de operador :
:- op(10, xfx, :).
:(X,Y).

% Base de datos de vuelos
horario( new_york, chicago,
           [  9:40 / 10:50 / nw4733 / todos,
             13:40 / 14:50 / nw4773 / habiles,
             19:40 / 20:50 / nw4833 / [lun,mar,mie,jue,vie,dom] ] ). 
horario( chicago, new_york,
           [  9:10 / 10:00 / nw458 / todos,
             12:20 / 13:10 / aa511 / todos ] ). 
             

horario( chicago, dallas,
           [  9:40 / 10:50 / aa4732 / todos,
             11:40 / 12:50 / aa4752 / habiles,
             18:40 / 19:50 / aa4822 / [lun,mar,mie,jue,vie] ] ).
horario( dallas, chicago,
           [ 12:00 / 14:00 / aa4823 / [lun,mar,mie,jue,vie] ] ).


horario( dallas, los_angeles,
           [ 13:20 / 16:20 / nw212 / [lun,mar,mie,vie,dom],
             16:30 / 19:30 / aa473 / [lun,mie,jue,sab] ] ). 
horario( los_angeles, dallas,
           [ 17:00 / 20:00 / nw213 / [lun,mar,mie,vie,dom],
             20:00 / 23:00 / aa474 / [lun,mie,jue,sab] ] ). 
             

horario( new_york, washington,
           [  9:10 / 11:45 / united614 / todos,
             14:45 / 17:20 / united805 / todos ] ). 
horario( washington, new_york,
           [ 12:00 / 14:35 / united616 / todos,
             17:45 / 20:20 / united807 / todos ] ). 
             

horario( chicago, miami,
           [  8:30 / 11:20 / nw510 / todos,
             11:00 / 13:50 / aa459 / todos ] ). 
horario( miami, chicago,
           [ 11:50 / 14:40 / nw520 / todos,
             14:00 / 16:50 / aa499 / todos ] ). 
             

horario( los_angeles, san_francisco,
           [ 11:30 / 12:40 / sw322 / [mar,jue] ] ). 
horario( san_francisco, los_angeles,
           [  9:25 / 10:15 / aa621 / todos,
             12:45 / 13:35 / sw623 / todos ] ). 


horario( san_francisco, seattle,
           [ 11:10 / 12:20 / sw211 / [lun,mar,mie,vie,dom],
             20:30 / 21:30 / nw472 / [lun,mie,jue,sab] ] ). 
horario( seattle, san_francisco,
           [ 7:55 / 8:45 / aa620 / todos,
             11:25 / 12:15 / aa666 / habiles ] ).


horario( dallas, san_francisco,
           [ 13:30 / 14:40 / nw323 / [mar,jue] ] ). 
horario( san_francisco, dallas,
           [ 15:00 / 16:10 / nw626 / [mar,jue] ] ). 


horario( boston, new_york,
           [ 9:00 / 9:40 / aa613 / [lun,mar,mie,jue,vie,sab],
            16:10 / 16:55 / united806 / [lun,mar,mie,jue,vie,dom] ] ). 
horario( boston, new_york,
           [ 10:00 / 10:40 / aa666 / [lun,mar,mie,jue,vie,sab],
            17:20 / 18:05 / united806 / [lun,mar,mie,jue,vie,dom] ] ). 
            
% Definicion de predicado dia para el uso de todos y habiles en horario.
dia(X,Y)       :- member(X,Y),!.
dia(X,todos)   :- member(X, [lun,mar,mie,jue,vie,sab,dom]),!.
dia(X,habiles) :- member(X, [lun,mar,mie,jue,vie]),!.

% Tiempos de transferencia segun el tipo de aeropuerto.
% 90: aeropuerto grande, 60: aeropuerto mediano, 40: aeropuerto pequeno
tiempo(X,90) :- member(X, [new_york,chicago,los_angeles]),!.
tiempo(X,60) :- member(X, [san_francisco,dallas,miami]),!.
tiempo(_,40).

% ruta( Ciudad1, Ciudad2, Dia, Ruta):
% Ruta es una secuencia de vuelos en el Dia, empenzado en Ciudad1, y finalizando en Ciudad2

% Caso base, cuando el vuelo es directo
ruta( P1, P2, Dia, [ P1 / P2 / Fnum / Deptime ] )  :-  
  vuelo( P1, P2, Dia, Fnum, Deptime, _).
  
  
% Caso cuando no hay vuelo directo y hay que gacer escalas
ruta( P1, P2, Dia, [ (P1 / P3 / Fnum1 / Dep1) | RestRuta] )  :- 
  append(RestRuta,_,[_,_,_]),              % Limita la cantidad de vuelos para no exceder la pila
  ruta( P3, P2, Dia, RestRuta),            % Existe un ruta entre las dos ciudades
  vuelo( P1, P3, Dia, Fnum1, Dep1, Arr1),  % Existe un vuelo del inicio a la escala
  salida( RestRuta, Dep2),                % Hora de salida del segundo vuelo                        
  transbordo( Arr1, Dep2, P2).             % Suficiente tiempo para hacer transbordo                           

vuelo( Ciudad1, Ciudad2, Dia, Fnum, Deptime, Arrtime)  :-
   horario( Ciudad1, Ciudad2, Flightlist),
   member( Deptime / Arrtime / Fnum / Dialist , Flightlist),
   dia( Dia, Dialist).

salida( [ P1 / P2 / Fnum / Dep | _], Dep).

transbordo( Hours1:Mins1, Hours2:Mins2, Ciudad)  :-
   tiempo(Ciudad,N),
   60 * (Hours2 - Hours1) + Mins2 - Mins1 >= N.
   
%transbordo( Hours1:Mins1, Hours2:Mins2)  :-
   %60 * (Hours2 - Hours1) + Mins2 - Mins1 >= 40.

%%% Permutations.
insertar(X,List, NewList) :- select(X, NewList, List).

perm1([],[]).
perm1([X|Xs], L) :- perm1(Xs,L1), insertar(X, L1, L).


% Interfaz grafica para el programa
ui :- write('Bienvenido al agente de viajes Prolog'), nl, 
        write('Que desea hacer:'), nl,
        write('1: Plan a ruta between two cities'), nl, 
        write('2: Take a tour'), nl,
        write('3: exit'),nl,
        read(X), process_choice(X).
