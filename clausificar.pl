/*Francisco M Garcia Sanchez-Belmonte
Angel Loro Mosqueda
Angel Sanchez Gonzalez*/

:- module(clausificar, [transformar/3]).
:- use_module(operadores).

/*Dada una lista con las premisas y la conclusion devuelve una lista de clausulas*/
transformar(Premisas,Conclusion,Clausulas) :-
    concatenarPremisasYnegarConclusion(Premisas,Conclusion,X0),
    elimimp(X0,Xl),
    negacion(Xl,X2),
    conjuncion(X2,X3),
    clausificar(X3,Clausulas,[]).
    
/*Toma una lista de premisas y las une mediante & y añade la conclusion negada*/
concatenarPremisasYnegarConclusion([],Conclusion,~Conclusion).
concatenarPremisasYnegarConclusion([H|T],Conclusion,H&Conc):-concatenarPremisasYnegarConclusion(T,Conclusion,Conc).
    
/*Mediante este predicado se eliminan los implicadores de la lista de premisas*/
elimimp((P<->Q),((P1 & Q1)#(~P1 & ~Q1))):-!,elimimp(P,P1),elimimp(Q,Q1).
elimimp((P->Q),(~P1 # Q1)):-!,elimimp(P,P1),elimimp(Q,Q1).
elimimp((P&Q),(P1 & Q1)):-!,elimimp(P,P1),elimimp(Q,Q1).
elimimp((P#Q),(P1 # Q1)):-!,elimimp(P,P1),elimimp(Q,Q1).
elimimp((~P),(~P1)):-!,elimimp(P,P1).
elimimp(P,P).
    
    
/*El predicado negacion se ocupa de devolver la formula con el proceso de negacion ya realizado */
negacion((~P),P1):- !, neg(P,P1).
negacion((P & Q),(P1 & Q1)):- !, negacion(P,P1), negacion(Q,Q1).
negacion((P # Q),(P1 # Q1)):- !, negacion(P,P1), negacion(Q,Q1).
negacion(P,P).
    
/*El predicado neg se ocupa de realizar la negacion de la formula*/
neg((~P),P1):-!,negacion(P,P1).
neg((P&Q),(P1#Q1)):-!,neg(P,P1),neg(Q,Q1).
neg((P#Q),(P1&Q1)):-!,neg(P,P1),neg(Q,Q1).
neg(P,(~P)).
 
/*Si tenemos como entrada (P # Q), primero debe poner P y Q en formas normales conjuntas, dicen P1 y Q1, y solo luego ver si la fórmula  es adecuado para la traducción por una de las equivalencias. El proceso debe ocurrir en este orden, porque puede suceder que ni P ni Q tenga & en el nivel superior, pero P1 y Q1 sí.*/
conjuncion((P#Q),R):-!,conjuncion(P,P1),conjuncion(Q,Q1),conjuncion1((P1 # Q1),R).
conjuncion((P&Q),(P1&Q1)):-!,conjuncion(P,P1),conjuncion(Q,Q1).
conjuncion(P,P).
    
conjuncion1(((P&Q)#R),(P1 & Q1)):-!,conjuncion((P#R),P1),conjuncion((Q#R),Q1).
conjuncion1((P#(Q & R)),(P1 & Q1)):-!,conjuncion((P#Q),P1),conjuncion((P#R),Q1).
conjuncion1(P,P).
    
/*Convierte una formula en forma clausal. 'clausificar' construye una representación interna de una lista de cláusulas, donde cada cláusula se representa como una estructura cl(A, B). En dicha estructura, A es la lista de literales que no se niegan, y B es la lista de literales que se niegan.*/
clausificar((P & Q),C1,C2) :-
    !, clausificar(P,C1,C3), clausificar(Q,C3,C2). 
clausificar(P,[cl(A,B)|Cs],Cs) :-
    inclause(P,A,[],B,[]),!.
clausificar(_,C,C).

inclause((P # Q),A,A1,B,B1):-
    !,inclause(P,A2,A1,B2,B1), inclause(Q,A,A2,B,B2).
inclause((~P),A,A,Bl,B) :-
    !, notin(P,A), putin(P,B,Bl).
inclause(P,A1,A,B,B) :- notin(P,B), putin(P,A,A1).

notin(X,[X|_]) :-!, fail.
notin(X,[_|L]):-!, notin(X,L).
notin(_,[]).

putin(X,[],[X]):- !.
putin(X,[X|L],[X|L]):- !.
putin(X,[Y|L],[Y|L1]):- putin(X,L,L1).
