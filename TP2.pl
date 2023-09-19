%
% Predicados 3.a - 3.e.
%
plano(11,11).
obstaculos([[2,2],[2,3],[3,2],[3,3],[3,8],[3,9],[4,8],[4,9],[7,4],[7,5],[7,6],[9,4],[9,5],[9,6]]).
puerta([1,6],[11,6],a).
puerta([11,6],[1,6],b).
puerta([6,11],[6,1],d).
puerta([6,1],[6,11],i).
puntaje(10).
wormCrecimiento(3).


%
% Algunos worms de ejemplo.
%
% worm([[6,2],[6,3],[6,4]])
% worm([[8,7],[9,7],[10,7],[11,7]])
%


%
% Predicado grafica/1 y auxiliares (NO MODIFICAR)
%
armaFila(0,[]):-!.
armaFila(M,[' '|T]):-M2 is M-1,armaFila(M2,T).
armaPlano(1,M,[T]):-armaFila(M,T2),append(T2,[1],T),!.
armaPlano(N,M,[T|TS]):-armaFila(M,T2),append(T2,[N],T),N2 is N-1,armaPlano(N2,M,TS).
muestraPlano([]):-write('12345678901'),nl.
muestraPlano([[X]|Z]):-write(X),nl,muestraPlano(Z).
muestraPlano([[X|Y]|Z]):-write(X),!,muestraPlano([Y|Z]).
cargaPlanoFil([P|PS],[1,M],C,[T|PS]):-cargaPlanoCol(P,M,C,T).
cargaPlanoFil([P|PS],[N,M],C,[P|TS]):-N>1,N2 is N-1,cargaPlanoFil(PS,[N2,M],C,TS).
cargaPlanoCol([_|PS],1,C,[C|PS]).
cargaPlanoCol([P|PS],M,C,[P|TS]):-M>1,M2 is M-1,cargaPlanoCol(PS,M2,C,TS).
cargaPlano(P,[X],C,P2):-cargaPlanoFil(P,X,C,P2),!.
cargaPlano(P,[X|Y],C,P3):-cargaPlanoFil(P,X,C,P2),cargaPlano(P2,Y,C,P3).
grafica(worm([X|Y])):-plano(N,M),
                      armaPlano(N,M,PL),
                      reverse(PL,P),
                      cargaPlano(P,[X],'H',P1),
                      cargaPlano(P1,Y,'O',P2),
                      obstaculos(O),
                      cargaPlano(P2,O,'#',P3),
                      muestraPlano(P3).
%
%--RESOLUCIÓN DEL TP:
%
% 4.a. avanzar/3
%
eliminarfinal(worm([[_,_]]),worm([])).
eliminarfinal(worm([C1|T]),worm([C1|T2])):- eliminarfinal(worm(T),worm(T2)).
movercabeza(worm([[X,Y]]),worm([[X2,Y2]]),b):- X2 is X+1, Y2 = Y.
movercabeza(worm([[X,Y]]),worm([[X2,Y2]]),a):- X2 is X-1, Y2 = Y.
movercabeza(worm([[X,Y]]),worm([[X2,Y2]]),i):- X2 = X, Y2 is Y-1.
movercabeza(worm([[X,Y]]),worm([[X2,Y2]]),d):- X2 = X, Y2 is Y+1.
avanzar(worm([Cabeza1|T]),worm([Cabeza2|T2]),D):- movercabeza(worm([Cabeza1]),worm([Cabeza2]),D),
                                                  eliminarfinal(worm([Cabeza1|T]),worm(T2)).
avanzar(worm([Cabeza1|T]),worm([Cabeza2|T2]),D):- puerta(Cabeza1,Cabeza2,D),
                                                eliminarfinal(worm([Cabeza1|T]),worm(T2)).
%
% 4.b. seguro/1
%
pertenece([X,Y],[[X,Y]]).
pertenece([X,Y],[[X,Y]|_]).
pertenece([X,Y],[[_,_]|T]):- pertenece([X,Y],T).
seguro(juego(worm([[X,Y]]),_)):-  plano(N,M),
                                  X>0,X=<N,Y>0,Y=<M,
                                  obstaculos(P),
                                  not(pertenece([X,Y],P)).
seguro(juego(worm([[X,Y]|T]),_)):-plano(N,M),
                                  X>0,X=<N,Y>0,Y=<M,
                                  obstaculos(P),
                                  not(pertenece([X,Y],P)),
                                  not(pertenece([X,Y],T)).
%
% 4.c movimiento/4
%
movimiento(juego(worm([Cabeza1|T]),C),juego(worm([Cabeza1|T]),C),0,[]).
movimiento(juego(worm([Cabeza1|T]),C),juego(worm([Cabeza2|T2]),C),1,[D]):- seguro(juego(worm([Cabeza1|T]),C)),
                                                                           avanzar(worm([Cabeza1|T]),worm([Cabeza2|T2]),D),
                                                                           seguro(juego(worm([Cabeza2|T2]),C)).
                                                                                          
movimiento(juego(worm([Cabeza1|T]),C),juego(worm([Cabeza2|T2]),C),N,[L|R]):- N>1,
                                                                             seguro(juego(worm([Cabeza1|T]),C)),
                                                                             avanzar(worm([Cabeza1|T]),worm([Aux|Taux]),L),
                                                                             seguro(juego(worm([Aux|Taux]),C)), N1 is N-1,
                                                                             movimiento(juego(worm([Aux|Taux]),C),juego(worm([Cabeza2|T2]),C),N1,R).
%
%4.d. intento/4
%
intento(juego(worm(W1),C),juego(worm(W2),C),N,L):- movimiento(juego(worm(W1),C),juego(worm(W2),C),N,L).
intento(juego(worm(W1),C),juego(worm(W2),C),N,L):- length(L,Largo),
                                                   Largo>N,
                                                   N1 is N+1,
                                                   intento(juego(worm(W1),C),juego(worm(W2),C),N1,L).

%
%4.e creceWorm/2
%
creceWorm(worm(W1),worm(W2)):-  wormCrecimiento(N),agrandarWorm(worm(W1),worm(W2),N).
agrandarWorm(worm([[X,Y]]),worm([[X,Y]]),0):-!.
agrandarWorm(worm([[X,Y]]),worm([[X,Y],[X,Y]]),1):-!.
agrandarWorm(worm([[X,Y]]),worm([[X,Y]|T]),N):- N>1,
                                                N1 is N-1,
                                                agrandarWorm(worm([[X,Y]]),worm(T),N1),!.
agrandarWorm(worm([Cabeza1|T]),worm([Cabeza1|T2]),N):- agrandarWorm(worm(T),worm(T2),N).
%
%4.f. jugar/4
%
jugar(J,J,[],0):-J=juego(_,comida([])),!.
jugar(juego(worm(W1),comida([C|R])),JF,[LD|Resto],P):- intento(juego(worm(W1),_),juego(worm([C|Taux]),_),1,LD),!,
                                              creceWorm(worm([C|Taux]),Waux),
                                              puntaje(Puntaje),
                                              jugar(juego(Waux,comida(R)),JF,Resto,PS),
                                              P is PS+Puntaje.

