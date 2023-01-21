%%% unify/2 unify(String,List).
%%% mi serve per dividere in una lista tutti gli argomenti della string json per esempio se c'è presente una stringa con degli spazi in mezzo unirà dal primo apice fino al secondo apice
trim(L,N,S) :-    
    length(P,N),   
    append(P,S,L).

subobject([],0,Object).
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
    N is 0,
    \+(A is '{'),
    \+(A is '}'),  
    !,
    subobject([], 0, [A | Object]).

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

jsonobj(['{' | Other]) :-
    jsonobj(Other).

jsonobj(['}']) :-
    jsonobj([]).

jsonobj([Attribute, ':', Value, D | Members]) :-
    string(Attribute),
    D == ',',
    !,
    (
        number(Value);
        string(Value);
        jsonobj(Value)
    ),
    jsonobj(Members).

jsonobj([Attribute, ':', Value, D | Members]) :-
    Value == '{',
    subobject([Value, D | Members],0,TrueValue),
    jsonobj(TrueValue),
    length(TrueValue, N),
    trim([Value, D | Members], N, NextMembers),
    append([Attribute, ':'], [TrueValue], Comp),
    append(Comp, NextMembers, VeryNextMembers),
    jsonobj(VeryNextMembers).    
    
%%% riconoscimento di jsonarray
jsonarray(Elements) :- 
    length(Elements, 0).

jsonarray([A, B| Elements]) :-
    B == ',',
    (
        number(A);
        string(A)
    ),
    !,
    jsonarray(Elements).

jsonarray([A, B| Elements]) :-
    B == ',',
    (
        number(A);
        string(A)
    ),
    !,
    jsonarray(Elements).

jsonarray([A | Elements]) :-
    A == '{',
    subobject([A | Elements],0,TrueValue),
    jsonobj(TrueValue),
    length(TrueValue, N),
    trim([A | Elements], N, NextElements),
    append(TrueValue, NextElements, VeryNextElements),
    jsonarray(VeryNextElements).

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

jsonparse(['{', Attribute, ':', Value , D | MoreMembers], [[Attribute, Value] | Object]) :-
    jsonobj([Attribute, ':', Value , D | MoreMembers]),
    string(Attribute),
    (
        string(Value);
        number(Value)
    ),
    !,
    jsonparse(MoreMembers, NewObject).

%%% usare subobj per sostituire value e renderlo una sotto lista jsonobj
jsonparse(['{', Attribute, ':', Value , D | MoreMembers], [[A, B, C] | Object]) :-
    jsonobj([Attribute, ':', Value , D | MoreMembers]),
    string(Attribute),
    Value == '{',
    !,
    subobject([Value, D | Members],0,TrueValue),
    jsonobj(TrueValue),
    length(TrueValue, N),
    N1 is N + 3,
    trim([Attribute, ':', Value, D | Members], N1, NextMembers), %%% da modificare
    jsonparse(MoreMembers, NewObject).

jsonparse(['[', Value, Virgola | MoreValues], [Value | Objects]) :-
    jsonarray([Value, Virgola | MoreValues]),
    Virgola == ',',
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



