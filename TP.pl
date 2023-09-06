%carga base de datos
inicio:-consult('datosTP.txt').
recomendar(Usuario):-
    buscarArtistas(Usuario,Art),
    assert(artistas(Art)),
    resumirLista(Art,Art2),
    /*genero(Gen,Cancion),
    animo(Ani,Cancion),
    duracion(Dur,Cancion),
    */
    atomic_list_concat(Art2,", ", Print),
    write_ln(Print).

%arma lista con todo los artistas
%escuchados por el usuario y cuantas veces escuchó cada canción
%la lista no esta resumida
buscarArtistas(Usuario,[H|T]):-
    dynamic(escuchas/3),
    escuchas(Usuario,Cancion,Cant), %busca canciones escuchadas por el usuario
    artista(Art,Cancion), %de esa canción busca el artista
    append([Art],[Cant],H), %concatena dos listas (artista y reproducciones)
    retract(escuchas(Usuario,Cancion,Cant)), %borra el hecho de memoria
    buscarArtistas(Usuario,T). %llama a recursividad.
buscarArtistas(_,[]). %fin de bucle.

%PARA BLUCE SUPERIOR
resumirLista([],[_]).
% resumirLista([[A,C]|T],B,[[A,S]]):-resumirLista(T,A,L),A=B,S is C+Z.
resumirLista([[A,C]|T],[[A,S]|L]):- sumarEscuchas([A,_],T,Z),S is C+Z, resumirLista(T,L).

sumarEscuchas(X, [],0).
sumarEscuchas([A,_], [[B,C]|T],S):- sumarEscuchas([A,_],T,Z),A=B,S is C+Z.
%sumarEscuchas([A,_], [_|T],S):- sumarEscuchas([A,_], T, Z),S is Z.

%pruebs fallidas
% replace(_,S,[],Y):-append([S],[],Y).
% replace(E,S,[E|T1],[S|T2]):-replace(E,S,T1,T2).
% %replace(E,S,[H|T1],[H|T2]):-E\=H, replace(E,S,T1,T2).
% replace(E,S,[H|T1],T2):-E\=H, replace(E,S,T1,T2).


%incompleto: de todas las canciones, se queda con las
%que el usuario no escucho y las printea
cancionesNuevas(Usuario):-
    dynamic(artista/2),
    artista(_,Cancion),
    genero(_,Cancion),
    animo(_,Cancion),
    duracion(_,Cancion),
    %U\=Usuario,*/
    not(escuchas(Usuario,Cancion,_)),
    %vuelve a preguntar por las que y escuchó, ya que no las estoy sacando.
    write(Cancion),
    retract(artista(_,Cancion)),
    cancionesNuevas(Usuario).
cancionesNuevas(_).


%incompleto: debe mostrar las canciones a reomendar
%el usuario elije cual, y la agrega a la base de conocimiento
menu(Usuario):-
    write_ln("Creo que estas canciones podrían gustarte"),
    write_ln("lista"),
    write_ln("¿Querés escuchar alguna?"),
    read(c),
    %obtener canción según indice elegido.
    assert(escuchas(Usuario,c,1)).
