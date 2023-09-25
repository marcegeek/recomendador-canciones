inicio:-
    write_ln('Ingrese su usuario para que le recomiende canciones de su gusto'),
    read(Usuario),
    cargarBase,
    assert(filtros([])),
    retractall(filtros/1),
    assert(filtros([])),
    write('¡Hola '), write(Usuario),write_ln('! ¿Que quieres buscar hoy? cancion, genero, animo o artista?'),
    writeln('----------------'),
    writeln('¡Tambien podemos mostrarte tendencias tuyas o novedades!;)'),
    read(Opcion),
    menu(Opcion,Usuario). %Agregar una cadena vacía a menú e ir filtradolá a medida que se use.
    %recomendar(Usuario).

%carga base de datos
cargarBase:-
     retractall(filtros/1)
    ,retractall(escuchas/3)
    ,retractall(genero/2)
    ,retractall(artista/2)
    ,retractall(duracion/2)
    ,retractall(animo/2)
    ,retractall(usuario/3)
    ,consult('datosTP.txt').


menu(Dato, Usuario) :-
    % Lógica para buscar canciones, artistas, géneros, ánimos, novedades, etc.
    (
        % Lógica del menú aquí
        (   Dato = cancion ->
            write('Ingrese una cancion '),
            read(Cancion),
            agregarFiltro(Cancion),
            buscarCancion(Cancion,Lista)
        ;   Dato = artista ->
            write('Indique un artista '),
            read(Artista),
            agregarFiltro(Artista),
            buscarArtista(Artista,Lista)
        ;   Dato = genero ->
            write('Ingrese un genero '),
            read(Genero),
            agregarFiltro(Genero),
            buscarGenero(Genero,Lista)
        ;   Dato = animo ->
            write('¿Como te sientes hoy? '),
            read(Ani),
            agregarFiltro(Ani),
            buscarAnimo(Ani, Lista)
        ;   Dato = novedades ->
            writeln('¡Aqui estan las canciones mas escuchadas!'),
            recomendar(Usuario)
        ),

        % Preguntar si se quiere agregar otro filtro
        write('¿Quieres agregar un filtro mas? (si/no): '),
        read(Opc),
        (
            Opc = 'si' ->
            % Si se responde "si", continuar el bucle
                write_ln('Ingrese el dato por le que buscar'),
                read(Data),
                menuFiltro(Data,Usuario,Lista)
            ; Opc = 'no' ->
                % Si se responde "no", salir del bucle
                elegirCanciones(Usuario,Lista)
            ;
                % Si la respuesta no es válida, mostrar un mensaje de error y continuar el bucle
                writeln('Respuesta no valida. Responde "si" o "no"')
        )
    ), inicio.

   %menuLoop(Usuario) :-
    %write_ln('Elige si buscar por Cancion, genero, animo o artista?'),
    %read(Dato),
    %menu(Dato, Usuario).

menuFiltro(Dato, Usuario,Lista) :-
    % Lógica para buscar canciones, artistas, géneros, ánimos, novedades, etc.
    (
        % Lógica del menú aquí
        (   Dato = cancion ->
            write('Ingrese una cancion '),
            read(Cancion),
            %agregarFiltro(Cancion),
            %buscarCancion(Cancion,Lista)
            filtraCancion(Cancion,Lista,ListaParcial)
        ;   Dato = artista ->
            write('Indique un artista '),
            read(Artista),
            %agregarFiltro(Artista),
            %buscarArtista(Artista,Lista)
            filtraArtista(Artista,Lista,ListaParcial)
        ;   Dato = genero ->
            write('Ingrese un genero '),
            read(Genero),
            %agregarFiltro(Genero),
            %buscarGenero(Genero,Lista)
            filtraGenero(Genero,Lista,ListaParcial)
        ;   Dato = animo ->
            write('¿Como te sientes hoy? ',ListaParcial),
            read(Ani),
            %agregarFiltro(Ani),
            %buscarAnimo(Ani, Lista)
            filtraAnimo(Ani,Lista,ListaParcial)
        ),

        % Preguntar si se quiere agregar otro filtro
        write('¿Quieres agregar un filtro mas? (si/no): '),
        read(Opc),
        (
            Opc = 'si' ->
            write_ln('Ingrese el dato por le que buscar'),
                read(Data),
                menuFiltro(Data,Usuario,ListaParcial)
           ; Opc = 'no' ->
                % Si se responde "no", salir del bucle
                elegirCanciones(Usuario,ListaParcial)
            ;
                % Si la respuesta no es válida, mostrar un mensaje de error y continuar el bucle
                writeln('Respuesta no valida. Responde "si" o "no"')
        )
    ), inicio.

filtraCancion(Cancion,[Cancion|T],[H|T2]):-
    artista(Cancion,_),
    retract(artista/2),
    append([Cancion],H),
    filtraCancion(Cancion,T,T2).
filtraCancion(Cancion,[_|T],T2):-
    filtraCancion(Cancion,T,T2).
filtraCancion(_,[],_).

filtraAnimo(Animo,[Cancion|T],[H|T2]):-
    animo(Cancion,Animo),
    retract(animo/2),
    append([Cancion],H),
    filtraAnimo(Animo,T,T2).
filtraAnimo(Animo,[Cancion|T],T2):-
    animo(Cancion,_),
    retract(animo/2),
    filtraAnimo(Animo,T,T2).
filtraAnimo(_,[],_).

filtraGenero(Genero,[Cancion|T],[H|T2]):-
    genero(Cancion,Genero),
    retract(genero/2),
    append([Cancion],H),
    filtraGenero(Genero,T,T2).
filtraGenero(Genero,[_|T],T2):-
    genero(_,_),
    retract(genero/2),
    filtraGenero(Genero,T,T2).
filtraGenero(_,[],_).

filtraArtista(Artista,[Cancion|T],[H|T2]):-
    artista(Cancion,Artista),
    retract(artista/2),
    append([Cancion],H),
    filtraArtista(Artista,T,T2).
filtraArtista(Artista,[Cancion|T],T2):-
    artista(Cancion,_),
    retract(artista/2),
    filtraArtista(Artista,T,T2).
filtraArtista(_,[],_).



agregarFiltro(Filtro) :-
    %retract(filtros(FiltrosActuales)),
    NuevaLista = [Filtro|_],
    assert(filtros(NuevaLista)),
    writeln('----------------'),
    writeln('Filtros actuales:'),
    mostrarFiltros(NuevaLista),
    writeln(''),
    writeln('----------------').
%agregarFiltro([]) :-
%    assert(filtros([])),
%    write_ln('Se eliminaron los filtros.').

mostrarFiltros([]).
mostrarFiltros([Filtro|Resto]) :-
    write(Filtro), write(', '),
    mostrarFiltros(Resto).

buscarAnimo(Animo,[Cancion|T]):-
    animo(Cancion,Animo),
    dynamic(artista/2),
    retract(artista(Cancion,_)),
    dynamic(genero/2),
    retract(genero(Cancion,_)),
    dynamic(animo/2),
    retract(animo(Cancion,_)),
    dynamic(duracion/2),
    retract(duracion(Cancion,_)),

    buscarAnimo(Animo,T).
buscarAnimo(_,[]). %fin de bucle.



buscarGenero(Genero,[Cancion|T]):-
    genero(Cancion,Genero),
    dynamic(artista/2),
    retract(artista(Cancion,_)),
    dynamic(genero/2),
    retract(genero(Cancion,_)),
    dynamic(animo/2),
    retract(animo(Cancion,_)),
    dynamic(duracion/2),
    retract(duracion(Cancion,_)),
    buscarGenero(Genero,T).
buscarGenero(_,[]). %fin de bucle.


buscarCancion(Cancion,[Cancion|T]):-
    artista(Cancion,_),

    dynamic(artista/2),
    retract(artista(Cancion,_)),
    dynamic(genero/2),
    retract(genero(Cancion,_)),
    dynamic(animo/2),
    retract(animo(Cancion,_)),
    dynamic(duracion/2),
    retract(duracion(Cancion,_)),
    buscarCancion(Cancion,T).
buscarCancion(_,[]). %fin de bucle.

buscarArtista(Artista,[Cancion|T]):-
    artista(Cancion,Artista),

    dynamic(artista/2),
    retract(artista(Cancion,_)),
    dynamic(genero/2),
    retract(genero(Cancion,_)),
    dynamic(animo/2),
    retract(animo(Cancion,_)),
    dynamic(duracion/2),
    retract(duracion(Cancion,_)),
    buscarArtista(Artista,T).
buscarArtista(_,[]). %fin de bucle.

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
    (   Eleccion = no ->
            inicio
    ;  Eleccion\='no' ->
            buscar_por_indice(Recomendaciones,Eleccion,Agregar),
            dynamic(escuchas/3),
            (escuchas(Usuario,Agregar,_)->
                (escuchas(Usuario,Agregar,CantActual),
                CantNueva is CantActual +1,
                retract(escuchas(Usuario,Agregar,CantActual))
                );
            CantNueva is 1
            ),
            assert(escuchas(Usuario,Agregar,CantNueva)),
            guardar
    ).


%elegirCanciones(_,[]):-
%    writeln('No se encontraron canciones, vuelva a intentarlo'),
%    inicio.

elegirCanciones(_,[]):-
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

mostrarCanciones([H|[]],1):-
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

mostrarCanciones([H|T],Pos):-
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
    listing(artista/2),
    listing(genero/2),
    listing(animo/2),
    listing(duracion/2),
    listing(escuchas/3),
    listing(usuario/3),
    told.

evaluarCancion(_,[],_,0).
evaluarCancion(Elemento,Lista,Factor,S):-
    reverse(Lista, ListaRev),
    buscar_por_elemento(Elemento,ListaRev,Val),
    S is Val * Factor.




buscar_por_indice([[H,_]|_], 1, H).
buscar_por_indice([H|_], 1, H).
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
    dynamic(escuchas/3),
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
    dynamic(artista/2),
    retract(artista(Cancion,Art)),
    dynamic(genero/2),
    retract(genero(Cancion,Gen)),
    dynamic(animo/2),
    retract(animo(Cancion,Ani)),
    dynamic(duracion/2),
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
    dynamic(artista/2),
    retract(artista(Cancion,Art)),
    dynamic(genero/2),
    retract(genero(Cancion,Gen)),
    dynamic(animo/2),
    retract(animo(Cancion,Ani)),
    dynamic(duracion/2),
    retract(duracion(Cancion,Dur)),


    evaluarCancion(Art,Artistas,1000,PuntajeArt),
    evaluarCancion(Gen,Generos,100,PuntajeGen),
    evaluarCancion(Ani,Animos,10,PuntajeAni),
    evaluarCancion(Dur,Duraciones,1,PuntajeDur),
    PuntajeTotal is PuntajeArt + PuntajeGen + PuntajeAni + PuntajeDur,

    append([Cancion],[PuntajeTotal],H_CancionesNuevas),
    cancionesNuevas(Usuario,Artistas,Generos,Animos,Duraciones,T_CancionesNuevas).
cancionesNuevas(_,_,_,_,_,[]).
