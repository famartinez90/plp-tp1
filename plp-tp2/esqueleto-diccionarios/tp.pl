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
% Funciona ya que member es de la forma member(?Elem, ?List), con lo cual
% el elem que le pasamos (en este caso: (A, _)) puede no estar completamente
% instanciado y por lo tanto member se encarga de unificar con variables
% libres donde corresponda, que este caso seria el "_". Asi es como genera
% un mapeo donde existen variables sin instanciar. 
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
% La idea en este ejercicio es a partir de las palabras de la lista
% separadas por espacios, asignarles variables e intentar mapearlas contra
% la lista de palabras de mi diccionario para encontrar combinaciones
% posibles.
% descifrar(+X, -Y)
descifrar(S, M):-
    descifrar_en_variables(S, Var_con_espacios),
    to_string(Var_con_espacios, Lista_strings),
    with_output_to(atom(M), maplist(write, Lista_strings)).

descifrar_en_variables(S, V):-
    palabras(S, Palabras),
    palabras_con_variables(Palabras, Variables),
    listas_de_diccionario(Variables),
    juntar_con(Variables, 32, V),
    length(S, Len_s),
    length(V, Len_var),
    Len_s == Len_var,
    cant_distintos(S, Elem_dist_s),
    cant_distintos(V, Elem_dist_var),
    Elem_dist_s == Elem_dist_var.

% to_string(-X, +Y)
to_string([], []).
to_string([L|Ls], [M|Ms]):- string_codes(M, [L]), to_string(Ls, Ms).

% listas_de_diccionario(-X)
listas_de_diccionario([L]):- diccionario_lista(L).
listas_de_diccionario([L|Ls]):- diccionario_lista(L), listas_de_diccionario(Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% La idea de este ejercicio es basicamente generar todos las posibles
% combinaciones de listas de la lista S con espacios intercalados usando
% la funcion con_espacios y luego siemplemente hacemos descifrar de las
% listas generadas
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% La idea para mensajes_mas_parejos es usar las funciones que usa descrifrar
% para generar los mensajes descrifrados posibles, luego obtener su desviacion
% estandar y, usando las funcion not, pedir que no exista otro mensaje
% descifrado tal que su desviacion estandar sea menor que el que encontramos
% al principio (X en el ejercicio). Este ejercicio puede tardar unos segundos
% en correr debido a todas las combinaciones posibles para el caso de
% dicc1.txt
% mensajes_mas_parejos(+S, -M)
mensajes_mas_parejos(S, M):-
    con_espacios(S, E),
    descifrar_en_variables(E, V),
    palabras_por_codigo(V, P),
    lista_longitudes(P, L),
    desviacion_estandar(L, X),
    not((con_espacios(S, F),
    descifrar_en_variables(F, W),
    palabras_por_codigo(W, Q),
    lista_longitudes(Q, K),
    desviacion_estandar(K, Y),
    Y < X)),
    to_string(V, Lista_strings),
    with_output_to(atom(M), maplist(write, Lista_strings)).

% Devuelve una lista con las longitudes de las listas
% de la lista de listas L
% lista_longitudes(+L, -R)
lista_longitudes([], []).
lista_longitudes([L|Ls], [R|Rs]):-
    length(L, R),
    lista_longitudes(Ls, Rs).

% Calcula la desviacion_estandar de una lista de numeros
% desviacion_estandar(+L, -SD)
desviacion_estandar(L, SD):-
    length(L, Longitud),
    sumlist(L, Sumatoria),
    Media is Sumatoria/Longitud,
    cuadrados(L, Media, Cuadrados),
    sumlist(Cuadrados, SumaCuadrados),
    Promedio is SumaCuadrados/Longitud,
    sqrt(Promedio, SD).

% Devuelve todos los numeros de una lista elevados al cuadrado
% cuadrados(+L, +M, -C)
cuadrados([], _, []).
cuadrados([E|L], M, [C|Cs]):- C is (E-M)*(E-M), cuadrados(L, M, Cs).

% palabras_por_codigo es equivalente a palabras pero separa por el codigo
% del espacio que es el 32
palabras_por_codigo([],[[]]).
palabras_por_codigo([L|Ls],[R|C]):- L \== 32, palabras_por_codigo(Ls,Rs), head(Rs,T),tail(Rs,C), append([L],T,R).
palabras_por_codigo([L|Ls],[[]|Rs]):- L == 32, palabras_por_codigo(Ls,Rs).
