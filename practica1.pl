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
suma_lista([X|Y], R) :-
    suma_lista(Y, R1), R is X + R1.



pertenece(X,[X|_]).
pertenece(X,[_|Y]):- pertenece(X,Y).

    % Generar las consultas
    % pertenece(c,[a,c,b,c,d]).
    % pertenece(X, [a,b,c]), pertenece(X, [d,c,b]).


conc([],L,L).
conc([X|R1],L,[X|R]):-
conc(R1,L,R).

    % I. ¿Qué lista hay que añadirle a la lista [a, b] para obtener [a, b, c, d]?
        % [c,d]
    % II. ¿Qué listas hay que concatenar para obtener [a, b]?
        %[a],[b]
    % III. ¿Pertenece b a la lista [a, b, c]?
        % conc(_,[b|_],[a,b,c]).  <-----
    % IV. ¿Es [b, c] una sublista de [a, b, c, d]?
        %
    % V. ¿Es [b, d] una sublista de [a, b, c, d]?
        %
    % VI. ¿Cuál es el último elemento de [b, a, c, d]?
        % con esto se consigue la cola conc(_,[_|L],[a,b,c]). 
        %

% palindromo es lo mismo escribir de adelnta a atras que de atras para adelante. ej: oso --->> [o,s,o].

palindromo([]).
palindromo([_]).
palindromo([X|Xs]) :-
    cola(Xs, U, Medio), X = U, palindromo(Medio).
%duplicar([],R).
%duplicar([X|Y],[X|X]):- duplicar(Y,[X|X]).
