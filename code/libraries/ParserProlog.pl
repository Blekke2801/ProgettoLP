%%% jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti

jsonparse("", []).

jsonparse(JSONString, Object) :- 
    string(JSONString),
    split_string(JSONString, "\s","\s", X), 
    jsonparse(X, Object),
    delete("\n", X, Xt).


jsonparse(Xt, Object) :- 
    Xo = "{",
    Xc = "}",
    Yo = "[",
    Yc = "]",   
    (Xt is [Xo | Members], last(Xt, Xc), jsonparse([Xo | Members], Object));
    (Xt is [Yo | Elements], last(Xt, Yc), jsonparse([Yo | Elements], Object)).

jsonparse([Xo | Members], Object) :- 
    selectchk([Xo | Members], Xo, New),
    reverse(New, R_new),
    selectchk(R_new, Xc, New2),
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
    jsonparse([MoreMembers], [Xo, [[Attribute, Punti, Value]], Xc]).

jsonparse([MoreMembers], [Xo, [[Attribute, Punti, Value]], Xc]) :-
    MoreMembers is [Pair | MoreMembers],
    jsonparse([Pair | MoreMembers], [Xo, [[Attribute, Punti, Value] | Other], Xc]).

jsonparse([Pair | MoreMembers], [Xo, [[Attribute, Punti, Value] | Other], Xc]) :-
    Object is [Xo, [[Attribute, Punti, Value] | Other], Xc],
    jsonparse([Pair | MoreMembers], Object).

jsonparse([Yo | Elements], Object) :- 
    selectchk([Yo | Elements], Yo, New),
    reverse(New, R_new),
    selectchk(R_new, Yc,  New2),
    reverse(New2, Elements),
    delete(",", X, Elementst).
    Elementst is [Value | MoreValues],
    jsonparse([Value | MoreValues], Object); 
    jsonparse([[]], Object).

jsonparse([Value | MoreValues], Object) :-
    (string(Value); Value is Object; number(Value)),
    jsonparse([MoreValues], [Yo, [Value | Other], Yc]).

jsonparse([MoreValues], [Yo, [Value | Other], Yc]) :-
    Object is [Yo, [Value | Other], Yc],
    jsonparse([Value | MoreValues], Object).

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

