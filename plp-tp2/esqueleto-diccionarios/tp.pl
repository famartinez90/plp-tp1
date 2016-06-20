:- dynamic(diccionario/1).
:- dynamic(diccionario_lista/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 & agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%diccionario_lista(+L).
    assertz(diccionario_lista(Codes)),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    read_file(Stream,L), !.

% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).

ej(4, [rombo, cuadrado, triangulo]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% juntar_con(?X, +Y, ?Z)
% No vamos a pedir que tanto X como Z esten o no instanciados. Sin embargo, el caso en que ninguno
% esta instanciado no es valido, dado que las soluciones son infinitas.
juntar_con([],_,[]).
juntar_con([L],_,L).
juntar_con([L|Ls],Y, Rs):- juntar_con(Ls,Y,Ss), append(L,[Y],R) , append(R,Ss,Rs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 3 PULIR DETALLES por ejemplo cuando tiene muchos espacios.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% palabras(+X, -Y)
% La decision de poner Y no instanciado se debe a que en nuestros ejercicios siempre
% nos viene como entrada una lista que no es del estilo lista de listas, con lo cual
% siempre buscamos quien es Y.
palabras([],[[]]).
palabras([L|Ls],[R|C]):- L \== espacio, palabras(Ls,Rs), head(Rs,T),tail(Rs,C), append([L],T,R).
palabras([L|Ls],[[]|Rs]):- L == espacio, palabras(Ls,Rs).

% head(+X, ?Y)
head([X|_],X).
% head(+X, ?Y)
tail([_|Xs],Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% asignar_var(+X, ?Y, ?Z)
% Mapeo inicial o final pueden venir o no instanciados, pero no ambos sin
% instanciar a la vez.
asignar_var(A,Mi,Mi):- member((A,_),Mi).
asignar_var(A,Mi,[(A,_)|Mi]):- not(member((A,_),Mi)).

% Pregunta?  Por que funciona asignar_var/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% palabras_con_variables(+X, -Y)
palabras_con_variables([],[]).
palabras_con_variables(Xs,Rs):-asignar_var_list_list(Xs,[],Mp), palabras_con_variables_aux(Xs,Rs,Mp).

palabras_con_variables_aux([],[],_).
palabras_con_variables_aux([X|Xs],[R|Rs],Mp):- palabra_con_variable(X,R,Mp), palabras_con_variables_aux(Xs,Rs,Mp).



palabra_con_variable([],[],_).
palabra_con_variable([X|Xs],[T|Ts],Mp):- member((X,T), Mp) , palabra_con_variable(Xs,Ts,Mp).


asignar_var_list_list([],Mi,Mi).
asignar_var_list_list([X|Xs],Mi,Mt):- asignar_var_list_list(Xs,Mi,Mf), asignar_var_list(X,Mf,Mt).


asignar_var_list([],Mi,Mi).
asignar_var_list([X|Xs],Mi,Mt):- asignar_var_list(Xs,Mi,Mf), asignar_var(X,Mf,Mt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quitar(+X, ?Y, ?Z)
% Al menos uno de Y y Z debe estar instanciado
quitar(_,[],[]).
quitar(E,[L|Ls],R):- E==L, quitar(E,Ls,R).
quitar(E,[L|Ls],[L|R]):- E\==L, quitar(E,Ls,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cant_distintos(+X, -Y)
cant_distintos([],0).
cant_distintos([L|Ls],N):- quitar(L,Ls,R), cant_distintos(R,M), N is M+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% descifrar(+X, -Y)
descifrar(S, M):-
    palabras(S, Palabras),
    palabras_con_variables(Palabras, Variables),
    listas_de_diccionario(Variables),
    juntar_con(Variables, 32, Var_con_espacios),
    length(S, Len_s),
    length(Var_con_espacios, Len_var),
    Len_s == Len_var,
    cant_distintos(S, Elem_dist_s),
    cant_distintos(Var_con_espacios, Elem_dist_var),
    Elem_dist_s == Elem_dist_var,
    to_string(Var_con_espacios, Lista_strings),
    with_output_to(atom(M), maplist(write, Lista_strings)).

% to_string(-X, +Y)
to_string([], []).
to_string([L|Ls], [M|Ms]):- string_codes(M, [L]), to_string(Ls, Ms).

% listas_de_diccionario(-X)
listas_de_diccionario([L]):- diccionario_lista(L).
listas_de_diccionario([L|Ls]):- diccionario_lista(L), listas_de_diccionario(Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% descifrar_sin_espacios(+S, -M)
descifrar_sin_espacios(S, M):-
    con_espacios(S, E),
    descifrar(E, M).

% con_espacios(+L, -R)
con_espacios(L, R):-
    length(L, X),
    Z is X*2-1,
    between(X, Z, Y),
    length(R, Y),
    mismos_elementos_con_espacios(L, R).

% mismos_elementos_con_espacios(+L, -M)
mismos_elementos_con_espacios([], []).
mismos_elementos_con_espacios(Ls, [espacio|Ms]):-
    mismos_elementos_con_espacios(Ls, Ms).
mismos_elementos_con_espacios([L|Ls], [L|Ms]):-
    mismos_elementos_con_espacios(Ls, Ms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%cosas que probablemente puedan servir para el ej 9 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lenght(?L,+X).
lenght([],0).
lenght([_|L],T):- T>0, T1 is T-1,lenght(L,T1).

%long(+L,?X).
long([],0).
long([_|L],T):- long(L,T1), T is T1+1.

%aplanar(+Ls,?R)
aplanar([],[]).
aplanar([[X|Xs]|Ls],R):-aplana([X|Xs],T),aplana(Ls,L),append(T,L,R),!.
aplanar([[]|Xs],R):-aplana(Xs,R),!.
aplanar([X|Xs],[X|R]):-aplana(Xs,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
