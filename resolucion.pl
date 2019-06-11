:-  op(900,  fy,  [ no ]).
:-  op(1050,  yfx,  [ -> , <-> ]).
:-  op(500,  yfx,  [ ^ , v ]).

atomo(A):-atom(A).
atomo(no(A)):-atom(A).

particion([],[]).
particion([H|T],L):-partir(H,H1),particion(T,T1),append([H1],T1,L).

clausalizar([],[]).
clausalizar([H|T],B):-clausal(H,H1),clausalizar(T,T1),append([H1],T1,B).

clausal(A -> B, C):-clausal(no(A) v B,C),!.
clausal(A <-> B, C):-clausal((A -> B) ^ (B -> A),C),!.
clausal(no(no(A)),B):-clausal(A,B),!.
clausal(no(A ^ B), C):-clausal(no(A) v no(B),C),!.
clausal(no(A v B), C):-clausal(no(A) ^ no(B),C),!.
clausal(no(A -> B),C):-clausal(no(no(A) v B),C),!.
clausal(no(A <-> B),C):-clausal(no((A -> B) ^ (B -> A)),C),!.
clausal(A v B, C v D):-clausal(A,C),clausal(B,D),!.
clausal(A ^ B, C ^ D):-clausal(A,C),clausal(B,D),!.
clausal(A,A):-atomo(A).

partir(A, [A]):-atomo(A).
partir(A ^ B,L):-
  partir(A, A1),
  partir(B, B1),
  append(A1,B1,L).

partir(A v B,L):-
  partir(A, A1),
  partir(B, B1),
  append(A1,B1,L).

/*Limpia duplicados. Aprovechamos que sort no los añade*/
limpiar(ENTRADA, SALIDA):- sort(ENTRADA, SALIDA).

inicializar(A, E):- clausalizar(A, C),particion(C,E).
esCorrecto(A):-inicializar(A,E),verificar(E).

printInfo(P1, P2, P3):-
  write(P1), write(" + "), write(P2), write(" = "), write(P3), nl.

verificar([]):-write("El argumento es correcto!"), nl,!.
verificar([A]):-(regla_de_resolucion(A,B),length(B,0)->write("El argumento es correcto!"), nl;write("El argumento es incorrecto!"), nl, fail),!.
verificar([P1, P2| COLA]):-regla_de_resolucion(P1,P2, S),limpiar(S,SALIDA),printInfo(P1,P2,SALIDA),
(length(SALIDA,0)->verificar(SALIDA);verificar([SALIDA|COLA])).

regla_de_resolucion(A,B,C):-append(A,B,AB),regla_de_resolucion(AB,C).
regla_de_resolucion(A,B):-limpiar(A,C),resolucion_core(C,C,B).

resolucion_core([],_,[]).
resolucion_core([H|T],A,L):-
  (eliminar(H,A)->resolucion_core(T,A,L);resolucion_core(T,A,L1),append([H],L1,L)).

eliminar(A,L):-member(no(A),L).
eliminar(no(A),L):-member(A,L).
