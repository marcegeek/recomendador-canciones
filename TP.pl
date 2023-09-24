:-dynamic(artista/2). %artista(titulo_cancion, nombre_artista).
:-dynamic(genero/2). %genero(titulo_cancion, nombre_genero).
:-dynamic(animo/2). %animo(titulo_cancion, tipo_animo).
:-dynamic(duracion/2). %duracion(titulo_cancion, tipo_duracion).
:-dynamic(escuchas/3). %escuchas(nombre_usuario, titulo_cancion, cantidad).
:-dynamic(usuario/3). %usuario(nombre_usuario, ciudad, pais).

inicio:-
    write_ln('Ingrese su usuario para que le recomiende canciones de su gusto'),
    read(Usuario),
    cargarBase,
    write('¡Hola '), write(Usuario),write_ln('! ¿Que quieres buscar hoy?¿Cancion, genero, animo o artista?'),
    write_ln('¡Tambien podemos mostrarte tendencias tuyas o novedades!;)'),
    read(Opcion),
    menu(Opcion,Usuario). %Agregar una cadena vacía a menú e ir filtradolá a medida que se use.
    %recomendar(Usuario).

%carga base de datos
cargarBase:-consult('datosTP.txt').

menu(Dato, Usuario):-
    %Falta agregar las tendencias propias y agregar los filtros a la búsqueda actual del usuario
    (   Dato = cancion ->
    write('Ingrese una cancion: '),
    read(Cancion),
    buscarCancion(Cancion,Art,Gen,Ani,Dur,Lista)
    ;   Dato = artista ->
    write('Indique un artista:'),
    read(Artista),
    buscarArtista(Artista,Gen,Can,Ani,Dur, Lista)
    ;   Dato = genero ->
    write('Ingrese un genero: '),
    read(Genero),
    buscarGenero(Genero,Art,Can,Ani,Dur,Lista)
    ;   Dato = animo ->
    write('¿Como te sientes hoy? '),
    read(Ani),
    buscarAnimo(Ani,Art,Can,Gen,Dur,Lista)

    ;   Dato = novedades ->
    writeln('Acá están tus canciones más escuchadas!'),
    recomendar(Usuario)),
    ordenar(Lista,ListaMuestra),

    %acá está la sección para agregar un filtro más
    write('¿Querés agregar algúna búsqueda más?'),
    read(Opc),
    (    Opc= agregar ->
    write_ln('¿Qué querés agregar?'),
    read(Agrega),
    agregarFiltro(Agrega,ListaMuestra,ListaFinal),
    ordenar(ListaFinal,ListaTerminada),
    elegirCanciones(Usuario,ListaTerminada)
    ;    Opc = no ->
    elegirCanciones(Usuario,ListaMuestra)),
    %elegirCanciones(Usuario,ListaMuestra),

    inicio.

%Me quedé trabado acá...
agregarFiltro(Agregar,Lista1,Lista2):-

    (   Agregar = cancion ->
    write('Ingrese una cancion: '),
    read(Cancion)
    ;   Agregar  = artista ->
    write('Indique un artista:'),
    read(Artista)
    ;   Agregar  = genero ->
    write('Ingrese un genero: '),
    read(Genero)
    ),
    ordenar(Lista,ListaMuestra).


buscarAnimo(Animo,Artistas,Canciones,Generos,Duraciones,[H|T]):-
    animo(Cancion,Animo),
    genero(Cancion,Gen),
    artista(Cancion,Art),
    duracion(Cancion,Dur),
    retract(artista(Cancion,Art)),
    retract(genero(Cancion,Gen)),
    retract(animo(Cancion,Animo)),
    retract(duracion(Cancion,Dur)),

    evaluarCancion(Art,Artistas,1000,PuntajeArt),
    evaluarCancion(Animo,Animos,10,PuntajeAni),
    evaluarCancion(Dur,Duraciones,1,PuntajeDur),
    PuntajeTotal is PuntajeArt + PuntajeAni + PuntajeDur,

    append([Cancion],[PuntajeTotal],H),
    buscarAnimo(Animo,Artistas,Canciones,Generos,Duraciones,T).
buscarAnimo(_,[],[],[],[],[]). %fin de bucle.



buscarGenero(Genero,Artistas,Canciones,Animos,Duraciones,[H|T]):-
    genero(Cancion,Genero),
    artista(Cancion,Art),
    animo(Cancion,Ani),
    duracion(Cancion,Dur),
    retract(artista(Cancion,Art)),
    retract(genero(Cancion,Genero)),
    retract(animo(Cancion,Ani)),
    retract(duracion(Cancion,Dur)),

    evaluarCancion(Art,Artistas,1000,PuntajeArt),
    evaluarCancion(Ani,Animos,10,PuntajeAni),
    evaluarCancion(Dur,Duraciones,1,PuntajeDur),
    PuntajeTotal is PuntajeArt + PuntajeAni + PuntajeDur,

    append([Cancion],[PuntajeTotal],H),
    buscarGenero(Genero,Artistas,Canciones,Animos,Duraciones,T).
buscarGenero(_,[],[],[],[],[]). %fin de bucle.


buscarCancion(Cancion,Artistas,Generos,Animos,Duraciones,[H|T]):-
    artista(Cancion,Art),
    genero(Cancion,Gen),
    animo(Cancion,Ani),
    duracion(Cancion,Dur),
    retract(artista(Cancion,Art)),
    retract(genero(Cancion,Gen)),
    retract(animo(Cancion,Ani)),
    retract(duracion(Cancion,Dur)),


    evaluarCancion(Art,Artistas,1000,PuntajeArt),
    evaluarCancion(Gen,Generos,100,PuntajeGen),
    evaluarCancion(Ani,Animos,10,PuntajeAni),
    evaluarCancion(Dur,Duraciones,1,PuntajeDur),
    PuntajeTotal is PuntajeArt + PuntajeGen + PuntajeAni + PuntajeDur,

    append([Cancion],[PuntajeTotal],H),
    buscarCancion(Cancion,Artistas,Generos,Animos,Duraciones,T).
buscarCancion(_,[],[],[],[],[]). %fin de bucle.

buscarArtista(Artista,Generos,Canciones,Animos,Duraciones,[H|T]):-
    artista(Cancion,Artista),
    genero(Cancion,Gen),
    animo(Cancion,Ani),
    duracion(Cancion,Dur),
    retract(artista(Cancion,Artista)),
    retract(genero(Cancion,Gen)),
    retract(animo(Cancion,Ani)),
    retract(duracion(Cancion,Dur)),


    evaluarCancion(Gen,Generos,100,PuntajeGen),
    evaluarCancion(Ani,Animos,10,PuntajeAni),
    evaluarCancion(Dur,Duraciones,1,PuntajeDur),
    PuntajeTotal is PuntajeGen + PuntajeAni + PuntajeDur,

    append([Cancion],[PuntajeTotal],H),
    buscarArtista(Artista,Canciones,Generos,Animos,Duraciones,T).
buscarArtista(_,[],[],[],[],[]). %fin de bucle.

ordenar([A|[]],[A]).
ordenar([[A,B],[C,D]|T],F):-
    (B<D ->
        (ordenar([[A,B]|T],L),
        append([[C,D]],L,F));
        (ordenar([[C,D]|T],L),
        append([[A,B]],L,F))).

elegirCanciones(Usuario,Recomendaciones):-
    write_ln('Creo que estas canciones podrian gustarte. Si no te interesan escribi No.'),
    reverse(Recomendaciones,RecRev),
    cargarBase,
    mostrarCanciones(RecRev,_),
    read(Eleccion),
    Eleccion\='No',
    buscar_por_indice(Recomendaciones,Eleccion,Agregar),
    assert(escuchas(Usuario,Agregar,1)),
    guardar.

elegirCanciones(_,_):-
        inicio.

mostrarCanciones([[H,_]|[]],1):-
    artista(H,Art),
    genero(H,Gen),
    animo(H,Ani),
    duracion(H,Dur),
    atom_concat(1, ': ', Print1),
    atom_concat(Print1, H, Print2),
    atom_concat(Print2, ' - ', Print3),
    atom_concat(Print3, Art, Print4),
    atom_concat(Print4, ' - ', Print5),
    atom_concat(Print5, Gen, Print6),
    atom_concat(Print6, ' - ', Print7),
    atom_concat(Print7, Ani, Print8),
    atom_concat(Print8, ' - ', Print9),
    atom_concat(Print9, Dur, Print),
    write_ln(Print).

mostrarCanciones([[H,_]|T],Pos):-
    mostrarCanciones(T,P),
    Pos is P+1,
    artista(H,Art),
    genero(H,Gen),
    animo(H,Ani),
    duracion(H,Dur),
    atom_concat(Pos, ': ', Print1),
    atom_concat(Print1, H, Print2),
    atom_concat(Print2, ' - ', Print3),
    atom_concat(Print3, Art, Print4),
    atom_concat(Print4, ' - ', Print5),
    atom_concat(Print5, Gen, Print6),
    atom_concat(Print6, ' - ', Print7),
    atom_concat(Print7, Ani, Print8),
    atom_concat(Print8, ' - ', Print9),
    atom_concat(Print9, Dur, Print),
    write_ln(Print).

guardar:-
    tell('datosTP.txt'),
    listing(artista),
    listing(genero),
    listing(animo),
    listing(duracion),
    listing(escuchas),
    told.

evaluarCancion(_,[],_,0).
evaluarCancion(Elemento,Lista,Factor,S):-
    reverse(Lista, ListaRev),
    buscar_por_elemento(Elemento,ListaRev,Val),
    S is Val * Factor.




buscar_por_indice([[H,_]|_], 1, H).
buscar_por_indice([_|T], I, Elemento) :-
    I > 1,
    I1 is I - 1,
    buscar_por_indice(T, I1, Elemento).

buscar_por_elemento(_, [], 0).
buscar_por_elemento(X, [[X,_]|_], 1).
buscar_por_elemento(X, [_|T], Pos):-
    buscar_por_elemento(X, T, Pos1),
    (Pos1=0->
        Pos is 0;
        Pos is Pos1 + 1).

recomendar(Usuario):-
    buscarEscuchadas(Usuario,Art,Gen,Ani,Dur),
    %assert(artistas(Art)),

    resumirLista(Art,ArtResumida),
    ordenar(ArtResumida,ArtOrdenada),
    resumirLista(Gen,GenResumida),
    ordenar(GenResumida,GenOrdenada),
    resumirLista(Ani,AniResumida),
    ordenar(AniResumida,AniOrdenada),
    resumirLista(Dur,DurResumida),
    ordenar(DurResumida,DurOrdenada),

    cancionesNuevas(Usuario, ArtOrdenada,GenOrdenada,AniOrdenada,DurOrdenada,A_recomendar),
    ordenar(A_recomendar,A_recomendarOrdenada),
    elegirCanciones(Usuario,A_recomendarOrdenada),
    inicio.

%arma lista con todo los artistas
%escuchados por el usuario y cuantas veces escuchó cada canción
%la lista no esta resumida
%ARMAR UN cancionesEscuchadas que se pase el escuchadas y que llame a los distintos buscarAtributo
buscarEscuchadas(Usuario,[H|T],[H1|T1],[H2|T2],[H3|T3]):-
    escuchas(Usuario,Cancion,Cant), %busca canciones escuchadas por el usuario
    artista(Cancion,Art),  %de esa canción busca el artista
    genero(Cancion,Gen),
    animo(Cancion,Ani),
    duracion(Cancion,Dur),
    append([Art],[Cant],H), %concatena dos listas (artista y reproducciones)
    append([Gen],[Cant],H1), %concatena dos listas (artista y reproducciones)
    append([Ani],[Cant],H2),
    append([Dur],[Cant],H3),
    retract(escuchas(Usuario,Cancion,Cant)), %borra el hecho de memoria
    retract(artista(Cancion,Art)),
    retract(genero(Cancion,Gen)),
    retract(animo(Cancion,Ani)),
    retract(duracion(Cancion,Dur)),
    buscarEscuchadas(Usuario,T,T1,T2,T3). %llama a recursividad
buscarEscuchadas(_,[],[],[],[]). %fin de bucle.


resumirLista([],[]).
resumirLista([[A,C]|T],[[A,S]|L]):-
    sumarEscuchas([A,_],T,T2,Z),
    S is C+Z,
    resumirLista(T2,L).


sumarEscuchas(_, [],[],0).
sumarEscuchas([A,_], [[A,C]|T],L,S):-
    sumarEscuchas([A,_],T,L,Z),
    S is C+Z.
sumarEscuchas([A,_], [B|T],[B|L],S):-
    sumarEscuchas([A,_], T,L, Z),
    S is Z.


cancionesNuevas(Usuario,Artistas,Generos,Animos,Duraciones,[H_CancionesNuevas|T_CancionesNuevas]):-
    artista(Cancion,Art),
    genero(Cancion,Gen),
    animo(Cancion,Ani),
    duracion(Cancion,Dur),
    retract(artista(Cancion,Art)),
    retract(genero(Cancion,Gen)),
    retract(animo(Cancion,Ani)),
    retract(duracion(Cancion,Dur)),


    evaluarCancion(Art,Artistas,1000,PuntajeArt),
    evaluarCancion(Gen,Generos,100,PuntajeGen),
    evaluarCancion(Ani,Animos,10,PuntajeAni),
    evaluarCancion(Dur,Duraciones,1,PuntajeDur),
    PuntajeTotal is PuntajeArt + PuntajeGen + PuntajeAni + PuntajeDur,

    append([Cancion],[PuntajeTotal],H_CancionesNuevas),
    cancionesNuevas(Usuario,Artistas,Generos,Animos,Duraciones,T_CancionesNuevas).
cancionesNuevas(_,_,_,_,_,[]).
