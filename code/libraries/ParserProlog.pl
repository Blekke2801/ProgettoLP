%%% jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti
jsonparse("", Object).
jsonparse(JSONString, Object) :- 
    string(JSONString),
    split_string(JSONString, "\s", "\s", X), jsonparse(X, Object).

jsonparse(X, Object) :-
    (Xo = "{", Xc = "}", X is [Xo | Members], last(X, Xc), jsonparse([Xo | Members], Object));
    (Yo = "[", Yc = "]", X is [Yo | Elements], last(X, Yc), jsonparse([Yo | Members], Object));

jsonparse([Xo | Members], Object) :- 
    (Members is [Pair | MoreMembers], jsonparse([[] | Xc], [Xo]); 
    jsonparse([[Pair | MoreMembers] | Xc], [Yo])).

jsonparse([[] | Xc], [Xo]) :- 
    jsonparse([], [Xo | Xc]).

jsonparse([[Pair | MoreMembers] | Xc], [Xo]) :- 
    Punti = ":",
    Pair is [Attribute | Punti]] | Value],
    jsonparse([[[[[Attribute | Punti]] | Value] | MoreMembers] | Xc], [Xo]).

jsonparse([[[[Attribute | Punti] | Value] | MoreMembers]  | Xc], [Xo]) :-
    string(Attribute),
    (string(Value); jsonobj(Value); number(Value)),
    jsonparse([Xc], [Xo | [[[Attribute | Punti] | Value] | MoreMembers]]).

jsonparse([Yo | Elements], Object) :-
    (jsonparse([[[Value| Virgola] | MoreValues] | Yc], [Yo]); jsonparse([[] | Yc], [Yo])).

jsonparse([[[Value| Virgola] | MoreValues] | Yc], [Yc]) :-
    (string(Value); jsonobj(Value); number(Value)),
    jsonparse([Yc], [Yo | [[Value| Virgola] | MoreValues]]).

jsonparse([[] | Yc], [Yo]) :- 
    jsonparse([Yc], [Yo | []]).

jsonparse([], [Xo | Xc]).
jsonparse([Yc], [Yo | [[Value| Virgola] | MoreValues]]).
jsonparse([], [Yo | Yc]).
jsonparse([Xc], [Xo | [[[Attribute | Punti] | Value] | MoreMembers]]).






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

