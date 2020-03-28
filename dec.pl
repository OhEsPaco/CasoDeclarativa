:-op(200,fx,~).
:-op(400,xfy,#).
:-op(400,xfy,&).
:-op(700,xfy,->).
:-op(700,xfy,<->).

transformar(X) :-
elimp(X,Xl), /* Fase 1 */
negacion(Xl,X2), /* Fase 2 */
skolem(X2,X3,[]), /* Fase 3 */
univout(X3,X4), /* Fase 4 */
conjn(X4,X5), /* Fase 5 */
clausify(X5,Clauses,[]), /* Fase 6 */
pclauses(Clauses). /* Imprimir Clausulas */

elimp((P<->Q),((P1 & Q1)#(~P1 & ~Q1))):-!,elimp(P,P1),elimp(Q,Q1).
elimp((P->Q),(~P1 # Q1)):-!,elimp(P,P1),elimp(Q,Q1).
elimp((P&Q),(P1 & Q1)):-!,elimp(P,P1),elimp(Q,Q1).
elimp((P#Q),(P1 # Q1)):-!,elimp(P,P1),elimp(Q,Q1).
elimp((~P),(~P1)):-!,elimp(P,P1).
elimp(P,P).

%% Moving Negation Inwards
negacion((~P),P1):- !, neg(P,P1).
negacion((P & Q),(P1 & Q1)):- !, negacion(P,P1), negacion(Q,Q1).
negacion((P # Q),(P1 # Q1)):- !, negacion(P,P1), negacion(Q,Q1).
negacion(P,P).
neg((~P),P1):-!,negacion(P,P1).
neg((P&Q),(P1#Q1)):-!,neg(P,P1),neg(Q,Q1).
neg((P#Q),(P1&Q1)):-!,neg(P,P1),neg(Q,Q1).
neg(P,(~P)).

%% Skolemising
skolem(all(X,P),all(X,P1),Vars):-!,skolem(P,P1,[X|Vars]).
skolem(exists(X,P),P2,Vars):- !, gensym(f,F),Sk=..[F|Vars],
							subst(X,Sk,P,P1),
							skolem(P1,P2,Vars).
skolem((P # Q),(P1 # Q1),Vars):-
						!,skolem(P,P1,Vars),skolem(Q,Q1,Vars).
skolem((P & Q),(P1 & Q1),Vars):-
						!,skolem(P,P1,Vars),skolem(Q,Q1,Vars).
skolem(P,P,_).

univout(all(_,P),P1):- !, univout(P,P1).
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
pclauses([cl(A,B) |Cs]) :-
pclause(A,B), nl, pclauses(Cs).
pclause(L,[]) :-!, pdisj(L), write('.').
pclause([],L) :-!, write(':-'), pconj(L), write('.').
pclause(Ll,L2) :-pdisj(Ll),write(' :- '), pconj(L2), write('.').
pdisj([L]) :- !, write(L).
%pdisj([L|Ls]) :- write(L), write(';'), pdisj(Ls). 
pdisj([L|Ls]) :- write(L), write(' # '), pdisj(Ls). 
pconj([L]):-!,write(L).
%pconj([L|Ls]):-write(L),write('.'),pconj(Ls).
pconj([L|Ls]):-write(L),write(' & '),pconj(Ls).