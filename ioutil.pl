%leer cadena por teclado como valor atom, eliminando espacios en blanco alrededor
leer_atom(A) :-
    read_string(user_input, "\n", "\r\t ", _, S), atom_string(A, S).

%"igualdad" de atoms, distinguiendo o no mayúsculas/minúsculas
%atoms_iguales(A1, A2). -> igualdad estricta por defecto
%atoms_iguales(S1, S2, CaseSensitive). -> caso con opción
atoms_iguales(A, A). %en estos dos casos se unifica directamente
atoms_iguales(A, A, true).
atoms_iguales(A1, A2, false) :-
    downcase_atom(A1, A1Lower), %comparamos todo en minúsculas
    downcase_atom(A2, A1Lower). %finalmente unificamos para comparar

mostrar_lista(L) :-
    mostrar_lista(L, _, _, _).
mostrar_lista(L, Sep) :-
    mostrar_lista(L, _, Sep, _).
mostrar_lista(L, Begin, End) :-
    mostrar_lista(L, Begin, _, End).
mostrar_lista(L, Begin, Sep, End) :-
    %determinar si se llama con alguna/s variable/s sin instanciar y
    %unificar con sus valores por defecto
    %formato por defecto: [e1, e2, e3, e4, ...]
    (   var(Begin) ->
        Begin = '['
    ;
        true
    ),
    (   var(Sep) ->
        Sep = ', '
    ;
        true
    ),
    (   var(End) ->
        End = ']'
    ;
        true
    ),
    write(Begin),
    mostrar_lista_interna(L, Sep),
    write(End).
mostrar_lista_interna([], _).
mostrar_lista_interna([H|[]], _) :-
    write(H).
mostrar_lista_interna([H|T], Sep) :-
    write(H), write(Sep),
    mostrar_lista_interna(T, Sep).

leer(Msg, TipoInput, TipoArgs, Datos) :-
    % Mostrar el mensaje de input e invocar TipoInput/2 (muestra indicador de ayuda)
    format('~w', Msg),
    call(TipoInput, TipoArgs),
    write(': '),
    leer_atom(Tmp),
    % Verificar y unificar los datos con TipoInput/4
    (   (call(TipoInput, Tmp, TipoArgs, Datos)) ->
        !
    ;
        % Los datos no son válidos, reintentar
        leer(Msg, TipoInput, TipoArgs, Datos)
    ).

leer_opcion(Msg, Opc, Permitidas) :- leer(Msg, opciones, Permitidas, Opc).

arbitrario(_).
arbitrario(Datos, _, Datos).

requerido(_) :- write(' (no dejar en blanco)').
requerido(Datos, _, Datos) :- Datos \= ''.

mostrar_opciones(L) :-
    mostrar_lista(L, '(', '/', ')').

opciones(Permitidas) :- write(' '), mostrar_opciones(Permitidas).
opciones(Opc, Permitidas, Opc) :- member(Opc, Permitidas).

numero(Rango) :-
    nonvar(Rango), !,
    format(' (número en el rango ~w)', [Rango]).
numero(_) :- write(' (número)').
numero(A, [Min, Max], N) :-
    nonvar(Min), nonvar(Max), !,
    numero(A, _, N), (N >= Min, N =< Max).
numero(A, _, N) :- atom_number(A, N).

numero_entero(Rango) :-
    nonvar(Rango), !,
    format(' (número entero en el rango ~w)', [Rango]).
numero_entero(_) :- write(' (número entero)').
numero_entero(A, [Min, Max], N) :-
    nonvar(Min), nonvar(Max), !,
    numero_entero(A, _, N), (N >= Min, N =< Max).
numero_entero(A, _, N) :- atom_number(A, N), integer(N).
