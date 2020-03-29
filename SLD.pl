/*Francisco M Garcia Sanchez-Belmonte
Angel Loro Mosqueda
Angel Sanchez Gonzalez*/

:- module(sld, [sld/1]).
:-use_module(operadores).

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