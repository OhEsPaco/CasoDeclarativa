%esCorrecto([[p,q,r],[no(p),q,r],[no(r)],[no(q)]]).
:- op(  555,  fx, [ no ]).

/*Comprueba si un argumento es correcto*/
esCorrecto(LIST):- elegir(LIST, P1), elegir(LIST, P2), regla_de_resolucion(P1,P2, SALIDA),
  (length(SALIDA,0)-> write("El argumento es correcto!"), nl;
  (length(P1, LP1),length(P2,LP2), length(SALIDA,LSALIDA),LSALIDA =:= LP1 + LP2 ->esCorrecto(LIST) ;esCorrecto([SALIDA|LIST]))).

prueba(LP1,LP2, LSALIDA):-LSALIDA =:= LP1 + LP2.
/*Elige aleatoriamente un elemento en una lista*/
elegir([], []).
elegir(List, Elt) :-
    length(List, Length),
    random(0, Length, Index),
    nth0(Index, List, Elt).

/*Aplica la regla de resolucion sobre dos listas de la forma [a,b,c]
que se traducen como a v b v c.
Primero las limpiamos para evitar duplicados.*/
regla_de_resolucion(PROD1,PROD2,SALIDA):-
  (atom(PROD1)->P1 = [PROD1]; P1 = PROD1),
  (atom(PROD2)->P2 = [PROD2]; P2 = PROD2),

regla_de_resolucion_iterar(P1,P2, SALIDA).

regla_de_resolucion_iterar([H|T], PROD, SALIDA):-
  resolucion_ent(H, PROD, SALIDA2),
  regla_de_resolucion_iterar(T, SALIDA2, SALIDA).

%regla_de_resolucion_iterar([],SALIDA,S2):-limpiar(SALIDA, S2).
regla_de_resolucion_iterar([],SALIDA,SALIDA).

/*Toma un elemento y una lista. Si la negación del elemento se encuentra en la
lista lo elimina. Si no se encuentra en la lista lo añade*/
resolucion_ent(ELEMENTO, ENTRADA, SALIDA):-
  (member(no(ELEMENTO), ENTRADA)->resolucion(ELEMENTO, ENTRADA, SALIDA);
  resolucion(ELEMENTO, ENTRADA, SALIDA_RESOLUCION),
  append([ELEMENTO], SALIDA_RESOLUCION, SALIDA)).

/*Toma un elemento y una lista. Si el elemento dado es 'a'
recorre la lista eliminando 'no(a)'*/
resolucion(_, [], []) :- !.
resolucion(X, [no(X)|Xs], Y) :- !, resolucion(X, Xs, Y).
resolucion(no(X), [X|Xs], Y) :- !, resolucion(no(X), Xs, Y).
resolucion(X, [T|Xs], Y) :- !, resolucion(X, Xs, Y2), append([T], Y2, Y).

/*Limpia duplicados. Aprovechamos que sort no los añade*/
limpiar(ENTRADA, SALIDA):- sort(ENTRADA, SALIDA).
