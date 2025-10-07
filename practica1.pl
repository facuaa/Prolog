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
palindromo(X):- reverse(X,R), R=X.
   
    
duplicar([],[]).
duplicar([X|Y],[X,X|R]):- duplicar(Y,R).



cuentaN(Lista, E, V) :-
    cuentaN(Lista, E, 0, Cant),
    V = Cant.  
                
cuentaN([], _, Acc, Acc).

cuentaN([X|Xs], E, Acc, Cant) :-
    X == E,
    Acc1 is Acc + 1,
    cuentaN(Xs, E, Acc1, Cant).

cuentaN([X|Xs], E, Acc, Cant) :-
    X \== E,
    cuentaN(Xs, E, Acc, Cant).



mayorN(_,[],[]).

mayorN(N,[X|XS],[X|YS]):- 
    X>N, 
    mayorN(N,XS,YS).

mayorN(N,[X|XS],YS):- 
    X=<N, 
    mayorN(N,XS,YS).


inversa(X,Y):-inversa(X,Y,[]).
inversa([X|Z],Y,T):-inversa(Z,Y,[X|T]).
inversa([],T,T).

inversa1(X,L):- inversa1(X,L,[]).
inversa1([],L,L).
inversa1([X|Y],L,T):- inversa1(Y,L,[X|T]).

antecesor1(X,Y):- padre_de(X, Y).
antecesor1(X,Y):- padre_de(X,Z),antecesor1(Z,Y).

antecesor2(X,Y):- padre_de(X,Z),antecesor2(Z,Y).
antecesor2(X,Y):- padre_de(X,Y).

antecesor3(X,Y):- padre_de(X,Y).
antecesor3(X,Y):- antecesor3(Z,Y), padre_de(X, Z).

antecesor4(X,Y):- antecesor4(Z,Y), padre_de(X, Z).
antecesor4(X,Y):- padre_de(X,Y).

familia(
persona([tomas,garcia,perez], fecha(7,mayo,1960), trabajo(profesor,60)),
persona([ana,lopez,ruiz], fecha(10,marzo,1962), trabajo(medica,90)),
[persona([juan,garcia,lopez], fecha(5,enero,1990), estudiante),
persona([maria, garcia, lopez], fecha(12,abril,1992), estudiante)]).
familia(
persona([jose,perez,ruiz], fecha(6,marzo,1963),trabajo(pintor,120)),
persona([luisa,galvez,perez], fecha(12,mayo,1964), trabajo(medica,90)),
[persona([juan_luis,perez,perez], fecha(5,febrero,1990), estudiante),
persona([maria_jose, perez, perez], fecha(12,junio,1992), estudiante),
persona([jose_maria, perez, perez], fecha(12,julio,1994), estudiante)]).
%_____________________________________________________________________________________________
%_____________________________________________________________________________________________
%_____________________________________________________________________________________________
%_____________________________________________________________________________________________
%_____________________________________________________________________________________________
padre(juan,maria).      %Juan es padre de maria
padre(juan,jose).       %Juan es padre de jose
padre(pedro,juan).      %Pedro es padre de juan

madre(ana,maria).       %Ana es madre de María
madre(ana,jose).        %Ana es madre de José
madre(luisa,pedro).     %Luisa es madre de Pedro

abuelo(X,Y):-
    padre(X,Z),
    padre(Z,Y).         %X es abuelo de Y si X es padre de Z y Z es padre de Y.

abuela(X,Y):-
    madre(X,Z),         
    madre(Z,Y).         %X es abuela de Y, si  X es mama de Z y Z es mama de Y.

bisABUELA(Z,X):-
    madre(Z,Y), 
    padre(Y,M),
    padre(M,X).

es_hijo(X,Y):- padre(Y,X).


mezcla([X|Xs],[Y|Ys],[X|Zs]):-X<Y,!,mezcla(Xs,[Y|Ys],Zs).
mezcla([X|Xs],[Y|Ys],[X,Y|Zs]):-X=Y,!,mezcla(Xs,Ys,Zs).
mezcla([X|Xs],[Y|Ys],[Y|Zs]):-X>Y,!,mezcla([X|Xs],Ys,Zs).
mezcla(Xs,[ ],Xs):-!.
mezcla([ ],Ys,Ys).

suma(0,0):- !.
suma(N,R):- A is N-1, suma(A,Ra), R is Ra+N.

%corte verde
rango(N,1):- N>0 , N=<2, !.   % 0 < N <= 2  ====>> 1
rango(N,2):- N>2 , N=<6, !.   % 2 < N <= 6  ====>> 2
rango(N,3):- N>6.             % N > 6  ====>> 3

%corte rojo
%rango1(N,1):- N>0 , N=<2, !.   % 0 < N <= 2  ====>> 1
%rango1(N,2):- N>2 , N=<6, !.   % 2 < N <= 6  ====>> 2
%rango1(N,3).                   % N > 6  ====>> 3




 %antes la hice sin acum,ahora la hago con acum y con recu cola.

suma_lista_recu_cola(Lista, Retorno):- suma_lista_recu_cola(Lista, 0, Retorno). 
suma_lista_recu_cola([],Acum,Acum):- !.
suma_lista_recu_cola([X|XS],Acum, S):- Nuevo is Acum + X, suma_lista_recu_cola(XS,Nuevo,S).

