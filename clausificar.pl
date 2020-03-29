/*Francisco M Garcia Sanchez-Belmonte
Angel Loro Mosqueda
Angel Sanchez Gonzalez*/

:- module(clausificar, [translate/3]).
:- use_module(operadores).

/*Dada una lista con las premisas y la conclusion devuelve una lista de clausulas*/
translate(Premises,Conclussion,Clauses) :-
    addPremisesAndNegateConclussion(Premises,Conclussion,X0), /* Stage 0*/
    elimimp(X0,Xl), /* Stage 1 */
    negacion(Xl,X2), /* Stage 2 */
    univout(X2,X4), /* Stage 3 */
    conjn(X4,X5), /* Stage 4 */
    clausify(X5,Clauses,[]). /* Stage 5 */
    
    /*Toma una lista de premisas y las une mediante & y añade la conclusion negada*/
    addPremisesAndNegateConclussion([],Conclussion,~Conclussion).
    addPremisesAndNegateConclussion([H|T],Conclussion,H&Conc):-addPremisesAndNegateConclussion(T,Conclussion,Conc).
    
    
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
    
	/*el predicado neg se ocupa de realizar la negacion de la formula*/
    neg((~P),P1):-!,negacion(P,P1).
    neg((P&Q),(P1#Q1)):-!,neg(P,P1),neg(Q,Q1).
    neg((P#Q),(P1&Q1)):-!,neg(P,P1),neg(Q,Q1).
    neg(P,(~P)).
    
    univout((P & Q),(P1 & Q1)) :-!, univout(P,P1), univout(Q,Q1). 
    univout((P#Q),(P1 # Q1)):-!,univout(P,P1),univout(Q,Q1).
    univout(P,P).
    
    conjn((P#Q),R):-!,conjn(P,P1),conjn(Q,Q1),conjn1((P1 # Q1),R).
    conjn((P&Q),(P1&Q1)):-!,conjn(P,P1),conjn(Q,Q1).
    conjn(P,P).
    
    conjn1(((P&Q)#R),(P1 & Q1)):-!,conjn((P#R),P1),conjn((Q#R),Q1).
    conjn1((P#(Q & R)),(P1 & Q1)):-!,conjn((P#Q),P1),conjn((P#R),Q1).
    conjn1(P,P).
    
    %% Putting into Clauses
    
    clausify((P & Q),C1,C2) :-
        !, clausify(P,C1,C3), clausify(Q,C3,C2). 
    
    clausify(P,[cl(A,B)|Cs],Cs) :-
        inclause(P,A,[],B,[]),!.
    clausify(_,C,C).
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
    
    
    pclauses([]) :- !, nl, nl.
    pclauses([cl(A,B) |Cs]) :-pclause(A,B), nl, pclauses(Cs).
    pclause(L,[]) :-!, pdisj(L), write('.').
    pclause([],L) :-!, write(':-'), pconj(L), write('.').
    pclause(Ll,L2) :-pdisj(Ll),write(' :- '), pconj(L2), write('.').
    pdisj([L]) :- !, write(L).
    pdisj([L|Ls]) :- write(L), write(' # '), pdisj(Ls). 
    pconj([L]):-!,write(L).
    pconj([L|Ls]):-write(L),write(' & '),pconj(Ls).
