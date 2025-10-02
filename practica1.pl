maximo(X,Y,M) :- (X >= Y -> M=X; M=Y).


primero([Y|_],Y).
    %primero([X, b, c], a).     -> X = a.
    %primero([X, Y], a).        -> X = a.
    %primero(X, a).             -> X = [a|_].

cola([_|X],X).
    %cola([a|L], [b, c]). -> L = [b, c].
    %cola(L, [b, c]). -> L = [_, b, c].
cons(E,A,[E|A]).
    %cons(X, [b, c], [a, b, c]).   -> X = a.
    %cons(a, L, [a, b, c]).        -> L = [b, c].
    %cons(b, L, [a, b, c]).        -> FALSE
    %cons(X, L, [a, b, c]).        ->X = a,
    %                                L = [b, c].
suma_lista([], 0).
suma_lista([X|Y], Total) :-
    suma_lista(Y, Subtotal),
    Total is X + Subtotal.



pertenece(X,[X|_]).
pertenece(X,[_|Y]):- pertenece(X,Y).

    % Generar las consultas
    % pertenece(c,[a,c,b,c,d]).
    % pertenece(X, [a,b,c]), pertenece(X, [d,c,b]).


conc([],L,L).

conc([X|R1],L2,[X|R]):-
 conc(R1,L2,R).