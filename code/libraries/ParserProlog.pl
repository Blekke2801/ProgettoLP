jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti
jsonparse().

jsonaccess/3 jsonaccess(Jsonobj, Field, Result).
%%% vero quando Result è recuperabile seguendo la catena di campi presenti in Fields
%%% (una lista) a partire da Jsonobj. Un campo rappresentato da N (con N un numero maggiore o
%%% uguale a 0) corrisponde a un indice di un array JSON.