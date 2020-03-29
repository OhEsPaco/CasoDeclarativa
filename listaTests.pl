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
	([p -> r , q <-> r,q & s, s->t,p], t, true),
	([p -> r , q <-> r,q & s, s->v,p], ~v, true),
	([p -> r->u , u & n ,n->m ,p->r], j-> r->m, true),
	([p <-> r , q <-> r,p], q, true),
	([p & r , q & r,q & t ,t -> ~ x ,x & ~ y,p],y , true),
	([x -> y , y #z,z->r, ~r ,x], y, true),
	([(~a -> ~a -> c) & c], ~a # c, true),
	([p # c & r, c -> p], ~a # p, true),
	([p -> q, p # (r & s)],t -> (~p -> s),true),
	([~p -> p],~q -> p,true),
	([p # (q # r),p -> (q # r),~r],q, true),
	([q -> p, (r # s) -> q, r],t->p,true)
	]).
