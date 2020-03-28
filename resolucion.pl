:-op(200,fx,~).
:-op(400,xfy,#).
:-op(400,xfy,&).
:-op(700,xfy,->).
:-op(700,xfy,<->).

/*Dada una lista con las premisas y la conclusion devuelve una lista de clausulas*/
translate(Premises,Conclussion,Clauses) :-
addPremisesAndNegateConclussion(Premises,Conclussion,X0), /* Stage 0*/
implout(X0,Xl), /* Stage 1 */
negin(Xl,X2), /* Stage 2 */
skolem(X2,X3,[]), /* Stage 3 */
univout(X3,X4), /* Stage 4 */
conjn(X4,X5), /* Stage 5 */
clausify(X5,Clauses,[]). /* Stage 6 */

/*Toma una lista de premisas y las une mediante & y añade la conclusion negada*/
addPremisesAndNegateConclussion([],Conclussion,~Conclussion).
addPremisesAndNegateConclussion([H|T],Conclussion,H&Conc):-addPremisesAndNegateConclussion(T,Conclussion,Conc).

implout((P<->Q),((P1 & Q1)#(~P1 & ~Q1))):-!,implout(P,P1),implout(Q,Q1).
implout((P->Q),(~P1 # Q1)):-!,implout(P,P1),implout(Q,Q1).
implout((P&Q),(P1 & Q1)):-!,implout(P,P1),implout(Q,Q1).
implout((P#Q),(P1 # Q1)):-!,implout(P,P1),implout(Q,Q1).
implout((~P),(~P1)):-!,implout(P,P1).
implout(P,P).

%% Moving Negation Inwards
negin((~P),P1):- !, neg(P,P1).
negin((P & Q),(P1 & Q1)):- !, negin(P,P1), negin(Q,Q1).
negin((P # Q),(P1 # Q1)):- !, negin(P,P1), negin(Q,Q1).
negin(P,P).
neg((~P),P1):-!,negin(P,P1).
neg((P&Q),(P1#Q1)):-!,neg(P,P1),neg(Q,Q1).
neg((P#Q),(P1&Q1)):-!,neg(P,P1),neg(Q,Q1).
neg(P,(~P)).

%% Skolemising
skolem((P # Q),(P1 # Q1),Vars):-
						!,skolem(P,P1,Vars),skolem(Q,Q1,Vars).
skolem((P & Q),(P1 & Q1),Vars):-
						!,skolem(P,P1,Vars),skolem(Q,Q1,Vars).
skolem(P,P,_).


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

%apuntad aqui los test que vayais haciendo
test1():-esArgumentoCorrecto([~ p -> q & r, s & q-> ~ r,t -> ~ u # s],t -> ~u # p).
test2():-esArgumentoCorrecto([p -> r , q <-> r,q & s, s->t,p],t).


%Dada una lista de premisas y una conclusion deduce si un argumento es correcto
esArgumentoCorrecto(Premisas,Conclussion):-translate(Premisas,Conclussion,Clauses),sld(Clauses).


%Dada una lista de clausulas ejecuta resolucion sobre ellas
sld(Clauses):-
	sld(Clauses,Clauses). %Hay que tener cuidado al generar un resolvente de que no esté entre las clasulas originales

sld([H|T],Clausulas):-
	resolver(H,T,Resolvente,Compatible),
	%Si el resolvente no es la clausula vacia seguimos
	((Resolvente\==cl([],[]))->
		%Si la clausula no es compatible no añadimos el resolvente
		((Compatible=no)->
		(sld(T,Clausulas));
		%La clausula es compatible
		(member(Resolvente,Clausulas)->
			%Si es miembro de las clausulas originales no añadimos el resolvente
			sld(T,Clausulas)
		%Si no es miembro de las clausulas originales añadimos el resolvente
		;sld([Resolvente|T],Clausulas) ))
	%Si el resolvente es la clausula vacia hemos acabado
	;nl,write(Resolvente),true).

%Toma una clausula y una lista de clausulas y devuelve el resolvente y si es compatible
%Compatible-> ha podido efectuar un paso de resolucion con alguna clausula de la lista
resolver(Clausula,[],Clausula,no):-!.

%Si la clausula es compatible la imprimimos y paramos
resolver(Clausula,[H|_],Resolvente,ClausulaCompatible):-
	resolverClausulas(Clausula,H,Resolvente,ClausulaCompatible),
	ClausulaCompatible=si,
	nl, write(Clausula), write(" + "), write(H), write(" = "), write(Resolvente),!.

%Si no es compatible seguimos
resolver(Clausula,[H|T],Resolvente,ClausulaCompatible):-
	resolverClausulas(Clausula,H,_,CC),
 	CC=no,
 	resolver(Clausula,T,Resolvente,ClausulaCompatible),!.

%Resuelve dos clausulas y nos dice si son compatibles, es decir, que se puede eliminar algo de alguna
resolverClausulas(cl(L1,L2),cl(L3,L4),Resolvente,ClausulaCompatible):-
	%Unimos las listas y eliminamos duplicados
	unirListas(L1,L3,Positivas), 
	unirListas(L2,L4,Negativas),
	%Eliminamos de las positivas las negativas y de las negativas las positivas
	eliminarOcurrencias(Positivas,Negativas,PositivasSinNegativas),
	eliminarOcurrencias(Negativas,Positivas,NegativasSinPositivas),
	%Generamos el resolvente
	Resolvente=cl(PositivasSinNegativas,NegativasSinPositivas),
	%Si las longitudes de ambas listas son las mismas antes y despues
	%de eliminar las ocurrencias es que no se ha podido quitar nada
	length(Positivas, L_Positivas),
	length(PositivasSinNegativas, L_PositivasSinNegativas),
	length(Negativas, L_Negativas),
	length(NegativasSinPositivas, L_NegativasSinPositivas),
	((L_Positivas=:=L_PositivasSinNegativas,L_Negativas=:=L_NegativasSinPositivas)->
		ClausulaCompatible=no;
		ClausulaCompatible=si).
	
	
%Quita de la lista A los elementos que aparezcan en la lista B
eliminarOcurrencias([],_,[]).
eliminarOcurrencias([H|T],L,Salida):-
	(member(H,L)->
		eliminarOcurrencias(T,L,Salida);
		(eliminarOcurrencias(T,L,L2),
	Salida=[H|L2])).

%Une dos listas y elimina los duplicados
unirListas(L1,L2,L3):- 
	append(L1,L2,L12),
	eliminarDuplicados(L12,L3).

%Elimina los duplicados de una lista
eliminarDuplicados([], []).
eliminarDuplicados([H | T], ListaSinDups) :- 
	member(H, T), 
	eliminarDuplicados(T, ListaSinDups),!.
eliminarDuplicados([H | T], [H | ListaSinDups]) :- 
	not(member(H, T)), 
	eliminarDuplicados(T, ListaSinDups),!.