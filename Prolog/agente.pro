% 3 Agente de viajes
% Patrick Rengifo 09-10703
% Daniela Rodriguez 09-10735

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
            
% dia(Dia,listaDias) :- predicado dia para el uso de todos y habiles en horario
% dia(dia,list) (?,?)
dia(X,Y)       :- member(X,Y).
dia(X,todos)   :- member(X, [lun,mar,mie,jue,vie,sab,dom]).
dia(X,habiles) :- member(X, [lun,mar,mie,jue,vie]).

% tiempo(Ciudad, Min) :-Tiempos de transferencia segun el tipo de aeropuerto.
% 90: aeropuerto grande, 60: aeropuerto mediano, 40: aeropuerto pequeno
% tiempo(ciudad, min) (?,?)
tiempo(X,90) :- member(X, [new_york,chicago,los_angeles]),!.
tiempo(X,60) :- member(X, [san_francisco,dallas,miami]),!.
tiempo(_,40).

% ruta(Origen, Destino, Dia, Ruta) :-
% Ruta es una secuencia de vuelos en el Dia, empenzado en Origen, y finalizando en Destino

% Caso base, cuando el vuelo es directo
ruta( Origen, Destino, Dia, [ Origen / Destino / Fnum / Deptime ] )  :-  
  vuelo( Origen, Destino, Dia, Fnum, Deptime, _).
  
% Caso cuando no hay vuelo directo y hay que gacer escalas
ruta( Origen, Destino, Dia, [ (Origen / P3 / Fnum1 / Dep1) | RestRuta] )  :- 
  append(RestRuta,_,[_,_,_]),                   % Limita la cantidad de vuelos para no exceder la pila
  ruta( P3, Destino, Dia, RestRuta),            % Existe un ruta entre las dos ciudades
  vuelo( Origen, P3, Dia, Fnum1, Dep1, Arr1),   % Existe un vuelo del inicio a la escala
  salida( RestRuta, Dep2),                      % Hora de salida del segundo vuelo                        
  transbordo( Arr1, Dep2, Destino).             % Suficiente tiempo para hacer transbordo                           

% vuelo(Origen, Destino, Dia, Fnum, Deptime, Arrtime) :- Triunfa si existe un
% un vuelo Fnum, desde Origen hasta Destino en el Dia que sale a las Deptime
% y llega a las Arrtime.
vuelo( Ciudad1, Ciudad2, Dia, Fnum, Deptime, Arrtime)  :-
   horario( Ciudad1, Ciudad2, Flightlist),
   member( Deptime / Arrtime / Fnum / Dialist , Flightlist),
   dia( Dia, Dialist).

% salida(Horario, Deptime) :- Determina la hora de salida del primer elemento
% de una lista de horarios.
salida( [ Origen / Destino / Fnum / Dep | _], Dep).

% transbordo(Hora1, Hora2, Ciudad) :- Triunfa si hay suficiente tiempo para 
% hacer escala en la ciudad con las horas indicadas.
transbordo( H1:M1, H2:M2, Ciudad)  :-
   tiempo(Ciudad,N),
   60 * (H2 - H1) + M2 - M1 >= N.

% element_at(Elem, List, Pos) :- Funciona como nth/3 pero contando desde 0.
element_at(X, List, Pos) :-
    element_at(X, List, 0, Pos).
element_at(X, [X|_], Pos, Pos).
element_at(X, [_|T], Acc, Pos) :-
    Acc1 is Acc + 1,
    element_at(X, T, Acc1, Pos).

% search(Elem, List, Rest) :- Triunfa al encontrar un Elem que al ser
% ser removido de List, resulta la lista Rest.
search(Elem, List, Rest) :-
        select(Elem, List, Rest0), !,
        Rest = Rest0.
% gira(Ciudades, Dias, Ruta) :- determina una serie de vuelo Ruta para
%recorrer una serie de Ciudades en tantos Dias cada una.
gira(Ciudades,Dias,Ruta) :- tour(Ciudades, Dia, Dias, Ruta).

%funcion auxiliar de gira
tour([_],_,_,[]).
tour(Ciudades, Dia, Dias, Ruta) :-
    search(C1, Ciudades, RestCiudades),
    search(C2, RestCiudades, _),
    sumar_dia(Dia, Dias, Nuevo),
    tour(RestCiudades,Nuevo,Dias,R2),
    vuelo(C1,C2,Dia,NumVuelo,Salida,Arrtime),
    append([C1,C2,Dia,NumVuelo,Salida],R2,Ruta).

% sumar_dia(Dia, Cantidad, Nuevo) :- Triunfa si el Dia al sumarle Cantidad,
% es igual a Nuevo.
sumar_dia(Dia, Cantidad, Nuevo) :- 
    element_at(Dia, [lun,mar,mie,jue,vie,sab,dom], X),
    Y is (X+Cantidad),
    Z is (Y mod 7),
    element_at(Nuevo, [lun,mar,mie,jue,vie,sab,dom], Z).

% Interfaz grafica para el programa
ui :- write('Bienvenido al agente de viajes Prolog'), nl, 
        write('Que desea hacer:'), nl,
        write('1: Planificar vuelo de A hasta B'), nl, 
        write('2: Que dias puedo viajar de A hasta B'), nl,
        write('3: En que dias puedo viajar entre A y B sin escalas'),nl,
        write('4: Como puedo hacer una gira entra A y B, con paradas de N dias'),nl,
        write('5: Como puedo viajar ida y vuelta entre A y B, con posibles paradas de N dias'),nl,
        write('Coloque un punto (.) al final del comando, i.e 1. '), nl,
        read(X), process_choice(X).
        
process_choice(1) :- write('Indique ciudad de origen'), nl, read(O), 
                      write('Indique ciudad de destino'), nl, read(D),   
                      ruta(O, D, Dia, Rutas),
                      write('Dia: '), write(Dia),nl,
                      write('Ruta: '), write(Rutas), nl.
                      
process_choice(2) :-  write('Indique ciudad de origen'), nl, read(O),
                      write('Indique ciudad de destino'), nl, read(D),
                      ruta(O, D, Dia, Rutas),
                      write('Dias: '), write(Dia),nl,
                      wirte('Ruta: '), write(Rutas), nl.
                      
process_choice(3) :-  write('Indique ciudad de origen'), nl, read(O),
                      write('Indique ciudad de destino'), nl, read(D),
                      vuelo(O, D, Dia, _, _, _),
                      write('Dias: '), write(Dia),nl.
                      
process_choice(4) :-  write('Indique lista de ciudades a visitar'), nl, read(L),
                      write('Indique cantidad de dias a estar cada dia'), nl, read(D),
                      gira(L, D, X),
                      write('Gira: '), write(X),nl.
                      
process_choice(5) :-  write('Indique las dos ciudades ida y vuelta'), nl, read(L),
                      gira(L, D, X),
                      write('Dias: '), write(D),nl,
                      write('Ruta: '), write(X),nl.