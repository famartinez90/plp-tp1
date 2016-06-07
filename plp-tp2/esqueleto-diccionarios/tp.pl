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
    assertz(diccionario_lista(Codes)),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    read_file(Stream,L), !.


% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
juntar_con([],_,[]).
juntar_con([L],_,L).
juntar_con([L|Ls],Y, Rs):- juntar_con(Ls,Y,Ss), append(L,[Y],R) , append(R,Ss,Rs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 3 PULIR DETALLES por ejemplo cuando tiene muchos espacios.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
palabras([],[[]]).
palabras([L|Ls],[R|C]):- L \== espacio, palabras(Ls,Rs), head(Rs,T),tail(Rs,C), append([L],T,R).
palabras([L|Ls],[[]|Rs]):- L == espacio, palabras(Ls,Rs).

head([X|_],X).
tail([_|Xs],Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
asignar_var(A,Mi,Mi):- member((A,_),Mi).
asignar_var(A,Mi,[(A,_)|Mi]):- not(member((A,_),Mi)).

% Pregunta?  Por que funciona asignar_var/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

quitar(_,[],[]).
quitar(E,[L|Ls],R):- E==L, quitar(E,Ls,R).
quitar(E,[L|Ls],[L|R]):- E\==L, quitar(E,Ls,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cant_distintos([],0).
cant_distintos([L|Ls],N):- quitar(L,Ls,R), cant_distintos(R,M), N is M+1.

% las devoluciones de panes se graban pero depende del formato. Si
% es franquicia o consecion no, no las graban al menos que autoricen la devolcion si? las de red
% si, son las que graban.

%Ejercicio 8

descifrar(S, M):- palabras(S, P), palabras_con_variables(P, V), listas_de_diccionario(V), juntar_con(V, 32, M).

listas_de_diccionario([L]):- diccionario_lista(L).
listas_de_diccionario([L|Ls]):- diccionario_lista(L), listas_de_diccionario(Ls).














