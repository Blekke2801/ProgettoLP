%%% unify/2 unify(String,List).
%%% mi serve per dividere in una lista tutti gli argomenti della string json per esempio se c'è presente una stringa con degli spazi in mezzo unirà dal primo apice fino al secondo apice
trim(L,N,S) :-    
    length(P,N),   
    append(P,S,L).

subobject(List,0,Object).
subobject([],N,[]).

subobject([A | List], N, [A | Object]) :-
    A is '{',
    !,
    N1 is N + 1,
    subobject(List, N1, Object).

subobject([A | List], N, [A | Object]) :-
    A is '}',
    !,
    N1 is N - 1,
    subobject(List, N1, Object).

subobject([A | List], N, [A | Object]) :-
    subobject(List, N, Object).


unify([], []).
unifyquotes([], []).

unify([A | As], List) :-
    A == '\"',
    !,
    unifyquotes(As, Ns),
    append([A], Ns, NewNs),
    length(NewNs, Int),
    trim([A | As], Int, NewAs),
    string_chars(String, NewNs),
    append([String], NewAs, L),
    unify(L, List).


unify([A | As], [A | List]) :-
    unify(As, List).

unifyquotes([A | As], [A | Ns]) :-
    A == '\"',
    !,
    unifyquotes([], []).
    
unifyquotes([A | As], [A | Ns]) :-
    unifyquotes(As,  Ns).

%%% jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti
%%% per distinguere array da oggetto mantenere le parentesi anche in object, forse, non lo so in realtà

%%% riconoscimento di jsonobj
jsonobj(Members) :- 
    length(Members, 0).

jsonobj([Attribute, ':', Value, D | Members]) :-
    string(Attribute),
    D == ',',
    !,
    (
        number(Value);
        string(Value)
    ),
    jsonobj(Members).

jsonobj([Attribute, ':', Value, D | Members]) :-
    Value == '{',
    jsonobj(TrueValue),
    jsonobj(Members).    
    
%%% riconoscimento di jsonarray
jsonarray(Elements) :- 
    length(Elements, 0).

jsonarray([A, B| Elements]) :-
    A is [Value],
    B is ",",
    (
        number(Value);
        string(Value)
    ),
    !,
    jsonobj(Members).

jsonarray([A | Members]) :-
    Value is ["{", TrueValue, "}"],
    jsonobj(TrueValue),
    jsonarray(Members).

%%% inizio scrittura di jsonparse
jsonparse("", []).
jsonparse([],[]).

jsonparse(JSONString, Object) :-
    atom(JSONString),
    atom_string(JSONString, String),
    !,
    string_chars(String, Chars),
    unify(Chars, Xt),
    jsonparse(Xt, Object).

jsonparse(JSONString, Object) :-
    string(JSONString),
    string_chars(JSONString, Chars),
    unify(Chars,Xt),
    delete(Xt, "\s", Xd),
    delete(Xd, "\n", Xs),
    jsonparse(Xs, Object).

jsonparse([], Object) :- 
    jsonparse([], []).

jsonparse(['{' | Members], Object) :-
    jsonobj(Members),
    !,
    jsonparse([A | Members], Object).

jsonparse(['{', Attribute, ':', Value , D | MoreMembers], [[Attribute, Value] | Object]) :-
    string(Attribute),
    (
        string(Value);
        number(Value)
    ),
    !,
    jsonparse(MoreMembers, NewObject).

%%% usare subobj per sostituire value e renderlo una sotto lista jsonobj
jsonparse(['{', Attribute, ':', Value , D | MoreMembers], [[A, B, C] | Object]) :-
    string(Attribute),
    Value is ['{', TrueValue, '}'], %%% da modificare
    jsonobj(TrueValue),
    jsonparse(MoreMembers, NewObject).

jsonparse([B | Other], Object) :-
    jsonarray(Members),
    !,
    jsonparse([B | Members], [B | Object]).

jsonparse(['[', Value, Virgola | MoreValues], [Value | Objects]) :-
    (
        string(Value);
        number(Value)
    ),
    !,
    jsonparse(MoreValues, Objects).

jsonparse(['[', Value, Virgola | MoreValues], [Value | Objects]) :-
    Value is ['{', TrueValue, '}'],
    jsonobj(TrueValue),
    jsonparse(MoreValues, Objects).

%%% jsonaccess/3 jsonaccess(Jsonobj, Fields, Result).
%%% vero quando Result è recuperabile seguendo la catena di campi presenti in Fields
%%% (una lista) a partire da Jsonobj. Un campo rappresentato da N (con N un numero maggiore o
%%% uguale a 0) corrisponde a un indice di un array JSON.

/*
jsonaccess("", [], Result) :- 
    jsonaccess("", [], []).

jsonaccess(Jsonobj,Fields,Result) :- 
    jsonparse(Jsonobj, Object),
    jsonaccess(Object, Fields,Result).

jsonaccess(Object, [Field | MoreFields], Result) :-
    Object is [Pair | MoreMembers],
    Pair is [Attribute , ':' , Value],
    jsonaccess([[Attribute , ':' , Value] | MoreMembers], [Field | MoreFields], Result).

jsonaccess([[Attribute , ':' , Value] | MoreMembers], [Field | MoreFields], Result) :-
    Attribute == Field,
    jsonaccess([MoreMembers], [MoreFields], [[Field, ':', Value]]).

*/



