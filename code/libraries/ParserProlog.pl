%%% jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti
jsonparse("", []).

jsonparse(JSONString, Object) :- 
    string(JSONString),
    split_string(JSONString, "\s", "\s", X), jsonparse(X, Object).

jsonparse(X, Object) :- 
    Xo = "{",
    Xc = "}",   
    (X is [Xo | Members], last(X, Xc), jsonparse([Xo | Members], Object));
    (X is [Yo | Elements], last(X, Yc), jsonparse([Yo | Elements], Object)).

jsonparse([Xo | Members], Object) :- 
    delete([Xo | Members], Xo, New),
    reverse(New, R_new),
    delete(R_new, Xc, New2),
    reverse(New2, Members),
    (Members is [Pair | MoreMembers], jsonparse([[]], Object); 
    jsonparse([Pair | MoreMembers], [])).

jsonparse([], Object) :- 
    jsonparse([], [Xo, Xc]).

jsonparse([Pair | MoreMembers], Object) :- 
    string_chars(Punti, ":"),
    Pair is [Attribute , Punti , Value],
    jsonparse([[Attribute , Punti , Value] | MoreMembers], Object).

jsonparse([[Attribute, Punti, Value] | MoreMembers], Object) :-
    string(Attribute),
    (string(Value); jsonobj(Value); number(Value)),
    jsonparse([], [Xo, [[Attribute, Punti, Value] | MoreMembers], Xc]).

jsonparse([Yo | Elements], Object) :- 
    delete([Yo | Elements], Yo, New),
    reverse(New, R_new),
    delete(R_new, Yc,  New2),
    reverse(New2, Elements),
    string_chars(Virgola, ","),
    Elements is [[Value, Virgola] | MoreValues],
    (jsonparse([[Value, Virgola] | MoreValues], Object); jsonparse([[]], Object)).

jsonparse([[[Value, Virgola] | MoreValues]], Object) :-
    (string(Value); Value is Object; number(Value)),
    jsonparse([], [Yo, [[Value, Virgola] | MoreValues], Yc]).

jsonparse([[]], Object) :- 
    jsonparse([], [Yo, Yc]).

jsonparse([], [Xo, [[Attribute, Punti, Value] | MoreMembers], Xc]).
jsonparse([Yc], [Yo | [[Value| Virgola] | MoreValues]]).
jsonparse([], [Yo, Yc]).
jsonparse([], [Yo, [[Value, Virgola] | MoreValues], Yc]).






%%% jsonaccess/3 jsonaccess(Jsonobj, Fields, Result).
%%% vero quando Result è recuperabile seguendo la catena di campi presenti in Fields
%%% (una lista) a partire da Jsonobj. Un campo rappresentato da N (con N un numero maggiore o
%%% uguale a 0) corrisponde a un indice di un array JSON.

/*jsonaccess(Object, [], Result).

jsonaccess(jsonobj(Members), Fields, Result) :- 
    jsonaccess([[[Attribute | ':'] | Value] | MoreMembers], [Field | MoreFields], Result),
    Attribute = Field.
jsonaccess([[[Attribute | ':'] | Value] | MoreMembers], [Field | MoreFields], Result), Attribute = Field :- 
    jsonaccess([MoreMembers], [MoreFields], [Value]).*/

