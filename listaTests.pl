/*Francisco M Garcia Sanchez-Belmonte
Angel Loro Mosqueda
Angel Sanchez Gonzalez*/

:- module(listaTests, [listaDeTests/1]).
:- use_module(operadores).

%Contiene una lista con los tests, los cuales siguen el siguiente formato
%(Lista de premisas, Conclusion, true/false)
%true si el argumento es correcto, false si no.
listaDeTests([
	([~ p -> q & r, s & q-> ~ r,t -> ~ u # s], t -> ~u # p, true),
	([p -> r , q <-> r,q & s, s->t,p], t, true)
	]).