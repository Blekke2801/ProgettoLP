Object = jsonobj(Members) ; jsonarray(Elements).
Members = [] ; [Pair | MoreMembers].
Pair = [Attribute | ": " | Value].
Attribute = string(X).
Value = string(X) ; Object ; number(X).

jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti
jsonparse(JSONString,Object) :- 


jsonaccess/3 jsonaccess(Jsonobj, Fields, Result).
%%% vero quando Result è recuperabile seguendo la catena di campi presenti in Fields
%%% (una lista) a partire da Jsonobj. Un campo rappresentato da N (con N un numero maggiore o
%%% uguale a 0) corrisponde a un indice di un array JSON.

jsonaccess(Jsonobj, Fields, Result) :- jsonparse(Jsonobj,Object), jsonaccess(Object, [], Result).
jsonaccess(jsonobj(Members), Fields, Result) :- jsonaccess([[Attribute | ": " | Value] | MoreMembers], [Field | MoreFields], Result), Attribute = Field.
jsonaccess([[Attribute | ": " | Value] | MoreMembers], [Field | MoreFields], Result), Attribute = Field :- jsonaccess([MoreMembers], [MoreFields], [Value]).

