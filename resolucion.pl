/*Francisco M Garcia Sanchez-Belmonte
Angel Loro Mosqueda
Angel Sanchez Gonzalez*/

:-use_module([operadores,sld,clausificar,listaTests]).

%Dada una lista de premisas y una conclusion deduce si un argumento es correcto
esArgumentoCorrecto(Premisas,Conclusion):-transformar(Premisas,Conclusion,Clausulas),sld(Clausulas).

%Ejecuta todos los tests de la lista
ejecutarTodosLosTests():-listaDeTests(L), ejecutarListaDeTests(L).

ejecutarListaDeTests([]).
ejecutarListaDeTests([H|T]):-
	nl,
	H = (Premisas,(Conclusion),Resultado), 
	write(Premisas), write(" |- "), write(Conclusion),
	((esArgumentoCorrecto(Premisas,Conclusion),nl)->(Resultado-> write("Test ok");write("Test fallido"))
	;(Resultado-> write("Test fallido");write("Test ok"))),nl,
	ejecutarListaDeTests(T).