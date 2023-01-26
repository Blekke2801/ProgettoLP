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
    N =:= 0,
    \+(A == '{'),
    \+(A == '}'),  
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

%%% inizio scrittura di jsonparse
jsonparse("", []).
jsonparse([],[]).
jsonparse(['{','}'],[]).
jsonparse(['[',']'],[]).

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

jsonparse(['{', Attr, ':', Value , D | Members], [[Attr, Value] | Object]) :-
    string(Attr),
    D == ',',
    !,
    (
        string(Value);
        number(Value)
    ),
    append(['{'], MoreValues, NextValues),
    jsonparse(Members, NewObject).

jsonparse(['{', Attr, ':', Value , D | Members], [[Attr, Value] | Object]) :-
    string(Attr),
    D == ']',
    length(['[', Value, D | MoreValues], Int),
    Int =:= 5,
    (
        number(Value);
        string(Value)
    ),
    jsonparse(['{','}'], []).

jsonparse(['{', Attr, ':', Value , D | Members], Object) :-
    string(Attr),
    Value == '{',
    \+(D == '}'),
    !,
    subobject(Members, 0, TrueValue),
    append([Value, D], TrueValue, Comp1),
    jsonparse(Comp1, ParsedValue),
    length(Comp1, N),
    N1 is N + 3,
    trim(['{', Attr, ':', Value, D | Members], N1, NextMembers),
    append(['{'], NextMembers, VeryNextMembers),
    append([Attr, ParsedValue], Object, NewObject),
    jsonparse(VeryNextMembers, NewObject).

jsonparse(['{', Attr, ':', Value , D | Members], Object) :-
    string(Attr),
    Value == '{',
    D == '}',
    !,
    append(['{'], Members, NextMembers),
    append([Attr, ['{','}']], Object, NewObject),
    jsonparse(NextMembers, NewObject).

jsonparse(['[',']'], Object) :- 
    jsonparse([], []).

jsonparse(['[', Value, D | MoreValues], [Value | Objects]) :-
    D == ',',
    !,
    (
        string(Value);
        number(Value)
    ),
    append(['['], MoreValues, NextValues),
    jsonparse(NextValues, Objects).

jsonparse(['[', Value, D | MoreValues], [Value | Objects]) :-
    D == ']',
    length(['[', Value, D | MoreValues], Int),
    Int =:= 3,
    (
        number(Value);
        string(Value)
    ),
    jsonparse(['[',']'], []).

jsonparse(['[', Value, D | MoreValues], Objects) :-
    Value == '{',
    \+(D == '}'),
    !,
    subobject(MoreValues, 0, TrueValue),
    append([Value, D], TrueValue, Comp1),
    jsonparse(Comp1, ParsedValue),
    length(Comp1, N),
    N1 is N + 1,
    trim(['[', Value, D | MoreValues], N1, NextValues),
    append(['['], NextValues, VeryNextValues),
    append([ParsedValue], Object, NewObject),
    jsonparse(VeryNextValues, NewObject).

jsonparse(['[', Value, D | MoreValues], Object) :-
    string(Attr),
    Value == '{',
    D == '}',
    !,
    append(['['], MoreValues, NextValues),
    append(['{','}'], Object, NewObject),
    jsonparse(NextValues, NewObject).

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
    Object is [Pair | Members],
    Pair is [Attr , ':' , Value],
    jsonaccess([[Attr , ':' , Value] | Members], [Field | MoreFields], Result).

jsonaccess([[Attr , ':' , Value] | Members], [Field | MoreFields], Result) :-
    Attr == Field,
    jsonaccess([Members], [MoreFields], [[Field, ':', Value]]).

*/



