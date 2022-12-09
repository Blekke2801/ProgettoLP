%%% conta l'occorrenza di un elemento in una lista

count([],Char, 0).

count([H|Tail], Char, N) :-
    char_code(H, Code),
    char_code(Char, Code),    
    count(Tail, Char, N1),
    N is N1 + 1.

count([H|Tail], Char, N) :-
    char_code(H, Code),
    \+char_code(Char, Code),
    count(Tail, Char, N).

%%% unify/2 unify(String,List).
%%% mi serve per dividere in una lista tutti gli argomenti della string json per esempio se c'è presente una stringa con degli spazi in mezzo unirà dal primo apice fino al secondo apice


unify([],List).
unifyquotes([],C).

unify([A | As], List) :-
    A == '\"',
    count(List, '\"', N),
    (N mod 2) =:= 0,
    unifyquotes(As, NewString),
    append(List, [NewString], NextList),
    unify(As, NextList).

unifyquotes([A | As], NewString) :-
    A == '\"',
    append(NewString, [A], NextList),
    append(['\"'], [NextList], LastList),
    string_chars(C, LastList),
    unifyquotes([], C).

unifyquotes([A | As], List) :-
    append(List, [A], NextList),
    unifyquotes(As, NextList).

unify([A | As], List) :-
    append(List, [A], NextList),
    unify(As, NextList).


%%% jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti

%%% riconoscimento di jsonobj
jsonobj(Members) :- 
    length(Members,0);
    (
        string(Attribute), 
        (
            string(Value); 
            number(Value); 
            jsonobj(Value)
        ),
        Pair is [Attribute, ':', Value], 
        Members is [Pair, ','| MoreMembers],
        jsonobj(MoreMembers)
    ).

%%%riconoscimento di jsonarray
jsonarray(Elements) :-
    length(Elements, 0); 
    (
        (
            string(Value); 
            jsonobj(Value); 
            number(Value) 
        ),
        Elements is [Value | MoreValues],
        jsonarray(MoreValues)
    ).


jsonparse("", []).

jsonparse(JSONString, Object) :- 
    (
        (
            atom(JSONString),
            atom_string(JSONString, String),
            string_chars(String, Chars)
        );
        (
            string(JSONString),
            string_chars(JSONString, Chars)
        )
    ),
    unify(Chars,Xt),
    jsonparse(Xt, Object).

%%% controllo delle parentesi all'inizio e alla fine
jsonparse(Xt, Object) :-  
    (
        nth0(0,Xt,'{'), 
        last(Xt, '}'), 
        Xt is [A | Other],
        jsonparse([A | Other], Object)
    );
    (
        nth0(0,Xt,'['), 
        last(Xt, ']'), 
        Xt is [B | Other],
        jsonparse([B | Other], Object)
    ).

jsonparse([A | Other], Object) :- 
    (
        jsonobj([A | Other]);
        (
            selectchk([A | Other], A, New),
            reverse(New, R_new), 
            selectchk(R_new, '}',  New2), 
            reverse(New2, Members), 
            jsonobj(Members)
        )
    ),
    jsonparse(jsonobj(Members), Object). 

jsonparse([], Object) :- 
    jsonparse([], []).

jsonparse(jsonobj(Members),Object) :- 
    jsonparse([[Attribute, ':', Value] | MoreMembers], Object).

jsonparse([[Attribute, ':', Value] | MoreMembers], Object) :-
    string(Attribute),
    (
        string(Value); 
        jsonobj(Value); 
        number(Value)
    ),
    append(Object, [[Attribute, ':', Value]], NewObject),
    jsonparse(MoreMembers, NewObject).

jsonparse(MoreMembers, NewObject) :-
    jsonobj(MoreMembers),
    jsonparse(jsonobj(MoreMembers), NewObject).

jsonparse([B | Other], Object) :- 
    (
        (   
            jsonarray([B | Other]),
            Elements is [B | Other] 
        );
        (
            selectchk([B | Other], B, New),
            reverse(New, R_new), 
            selectchk(R_new, ']',  New2), 
            reverse(New2, Elements), 
            jsonarray(Elements)
        )
    ),
    jsonparse(jsonarray(Elements), Object).
    
jsonparse(jsonarray(Elements),Object) :- 
    jsonparse([Value | MoreValues], Object).

jsonparse([Value | MoreValues], Object) :-
    (
        string(Value); 
        Value is Object; 
        number(Value)
    ),
    append(Object, [Value], NewObject),
    jsonparse(MoreValues, NewObject).

jsonparse(MoreValues, NewObject) :-
    jsonarray(MoreValues),
    jsonparse(jsonarray(MoreValues), NewObject).

jsonparse([], NewObject).
jsonparse([], []).






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



