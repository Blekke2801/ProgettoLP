%%% inizio funzioni di utility

%%% addquotes/2 addquotes([A | List], [B | Result]) 
%%% aggiunge 2 doppi apici ai confini della stringa, questa funzione serve perché 
%%% atomics_to_string trasformando una lista in una stringa, le stringhe all'interno perdono 
%%% i doppi apici, che servono per la riconversione
addquotes([], []).

addquotes([A | List], [B | Result]) :-
    string(A),
    !,
    append([['\"'], [A],['\"']], Comp),
    atomics_to_string(Comp, B),
    addquotes(List, Result).

    addquotes([A | List], [A | Result]) :-
        addquotes(List, Result).

%%% compare_list/2 compare_list([L1Head | L1Tail], List2)
%%% compara 2 liste se sono uguali
compare_list([],[]).
compare_list([],_).

compare_list([L1Head | L1Tail], List2) :-
    member(L1Head, List2),
    compare_list(L1Tail, List2).

%%% trim elimina tot elementi allinizio di una lista utlizzando l'append
trim(L,N,S) :-    
    length(P,N),   
    append(P,S,L).

%%% subobj/2 subobj([A | List], [A | Object])
%%% prendendo in input una lista con agli estremi delle parentesi graffe, crea una lista contenente un sotto-oggetto
subobj([], []).

subobj(['{' | List], [['{' | Sub] | _]) :-
    subobj(List, Sub).

subobj(['[' | List], [['[' | Sub] | _]) :-
    subarray(List, Sub).

subobj(['}' | _], ['}']) :-
    !.

subobj([A | List], [A | Object]) :-
    \+(A == ']'),
    \+(A == '['),
    \+(A == '}'),
    \+(A == '{'),
    subobj(List, Object).

%%% subarray/2 subarray([A | List], [A | Object])
%%% subarray in input una lista con agli estremi delle parentesi quadrate, crea una lista contenente un sotto-array
subarray([], []).

subarray(['[' | List], [['[' | Sub] | _]) :-
    subarray(List,  Sub).

subarray(['{' | List], [['{' | Sub] | _]) :-
    subobj(List, Sub).

subarray([']' | _], [']']) :-
    !.

subarray([A | List], [A | Object]) :-
    \+(A == ']'),
    \+(A == '['),
    \+(A == '}'),
    \+(A == '{'),
    subarray(List, Object).

%%% unify/2 unify(CharList, UnifiedList)
%%% prendendo una lista di caratteri come input, unisce tutti i caratteri tra doppi apici trasformandoli in una stringa unica, utilizzando unifyquotes
unify([], []).
unifyquotes([], []).
unifynumbers([], []).

unify(['\"' | As], List) :-
    unifyquotes(As, Ns),
    length(Ns, Int),
    Int1 is Int + 2,
    trim(['\"' | As], Int1, NewAs),
    string_chars(String, Ns),
    append([String], NewAs, L),
    unify(L, List).

unify([A | As], List) :-
    \+(number(A)),
    atom_number(A,_),
    !,
    unifynumbers([A | As], Ns),
    length(Ns, Int),
    trim([A | As], Int, NewAs),
    number_chars(Number, Ns),
    append([Number], NewAs, L),
    unify(L, List).

unify([A | As], [A | List]) :-
    unify(As, List).

unifyquotes(['\"' | _],[]) :-
    !.

unifyquotes([A | As], [A | Ns]) :-
    unifyquotes(As,  Ns).    

unifynumbers([A | As], [A | Ns]) :-
    atom_number(A,_),
    !,
    unifynumbers(As,  Ns).

unifynumbers(['.' | As], ['.' | Ns]) :-
    unifynumbers(As,  Ns).

unifynumbers(_, []) :-
    !.
 
%%% fine funzioni di utility


%%% jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti
%%% per distinguere array da oggetto mantenere le parentesi anche in object, forse, non lo so in realtà

%%% inizio scrittura di jsonparse
jsonparse("", [null]).
jsonparse([],[null]).
jsonparse(['{','}'],[]).
jsonparse(['[',']'],[]).

jsonparse(JSONString, [jsonobj | Object]) :-
    atom(JSONString),
    atom_chars(JSONString, Chars),
    unify(Chars, Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', [X | Xf]),
    X == '{',
    !,
    jsonparse([X | Xf], Object).

jsonparse(JSONString, [jsonarray | Object]) :-
    atom(JSONString),
    !,
    atom_chars(JSONString, Chars),
    unify(Chars, Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', [X | Xf]),
    X == '[',
    jsonparse([X | Xf], Object).

jsonparse(JSONString, [jsonobj | Object]) :-
    string(JSONString),
    string_chars(JSONString, Chars),
    unify(Chars,Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', [X | Xf]),
    X == '{',
    !,
    jsonparse([X | Xf], Object).

jsonparse(JSONString, [jsonarray | Object]) :-
    string(JSONString),
    string_chars(JSONString, Chars),
    unify(Chars,Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', [X | Xf]),
    X == '[',
    jsonparse([X | Xf], Object).

jsonparse(['{', Attr, ':', Value , '}'], [[Attr, Value]]) :-
    string(Attr),
    (
        number(Value);
        string(Value)
    ),
    !.

jsonparse(['[', Value, ']'], [Value]) :-
    (
        number(Value);
        string(Value)
    ),
    !.

jsonparse(['{', Attr, ':', Value , ',' | Members], [[Attr, Value] | Object]) :-
    string(Attr),
    (
        string(Value);
        number(Value)
    ),
    jsonparse(['{' | Members], Object).

jsonparse(['{', Attr, ':', '{' | Members], [[Attr, [jsonobj | ParsedValue]]]) :-
    string(Attr),
    subobj(Members, TrueValue),
    length(TrueValue, N),
    N1 is N + 4,
    trim(['{', Attr, ':', '{' | Members], N1, NextMembers),
    jsonparse(['{' | TrueValue], ParsedValue),
    compare_list(['{' | NextMembers], ['[', ']']),
    !.

jsonparse(['{', Attr, ':', '[' | Members], [[Attr, [jsonarray | ParsedValue]]]) :-
    string(Attr),
    subarray(Members, TrueValue),
    length(TrueValue, N),
    N1 is N + 4,
    trim(['{', Attr, ':', '[' | Members], N1, NextMembers),
    jsonparse(['[' | TrueValue], ParsedValue),
    compare_list(['{' | NextMembers], ['[', ']']),
    !.

jsonparse(['{', Attr, ':', '{' | Members], [[Attr, [jsonobj | ParsedValue]] | Object]) :-
    string(Attr),
    subobj(Members, TrueValue),
    length(TrueValue, N),
    N1 is N + 4,
    trim(['{', Attr, ':', '{' | Members], N1, NextMembers),
    jsonparse(['{' | TrueValue], ParsedValue),
    jsonparse(['{' | NextMembers], Object).

jsonparse(['{', Attr, ':', '[' | Members], [[Attr, [jsonarray | ParsedValue]] | Object]) :-
    string(Attr),
    subarray(Members, TrueValue),
    length(TrueValue, N),
    N1 is N + 4,
    trim(['{', Attr, ':', '[' | Members], N1, NextMembers),
    jsonparse(['[' | TrueValue], ParsedValue),
    jsonparse(['{' | NextMembers], Object).

jsonparse(['[', Value, ',' | MoreValues], [Value | Objects]) :-
    (
        string(Value);
        number(Value)
    ),
    jsonparse(['[' | MoreValues], Objects).

jsonparse(['[', '{' | MoreValues], [[jsonobj | ParsedValue]]) :-
    subobj(MoreValues, TrueValue),
    length(TrueValue, N),
    N1 is N + 2,
    trim(['[', '{', MoreValues], N1, NextValues),
    jsonparse(['{' | TrueValue], ParsedValue),
    compare_list(['[' | NextValues], ['[', ']']),
    !.

jsonparse(['[', '[' | MoreValues], [[jsonarray | ParsedValue]]) :-
    subarray(MoreValues, TrueValue),
    length(TrueValue, N),
    N1 is N + 2,
    trim(['[', '[' | MoreValues], N1, NextValues),
    jsonparse(['[' | TrueValue], ParsedValue),
    compare_list(['[' | NextValues], ['[', ']']),
    !.

jsonparse(['[', '{' | MoreValues], [[jsonobj | ParsedValue] | Object]) :-
    subobj(MoreValues, TrueValue),
    length(TrueValue, N),
    N1 is N + 2,
    trim(['[', '{', MoreValues], N1, NextValues),
    jsonparse(['{' | TrueValue], ParsedValue),
    jsonparse(['[' | NextValues], Object).

jsonparse(['[', '[' | MoreValues], [[jsonarray | ParsedValue] | Object]) :-
    subarray(MoreValues, TrueValue),
    length(TrueValue, N),
    N1 is N + 2,
    trim(['[', '[' | MoreValues], N1, NextValues),
    jsonparse(['[' | TrueValue], ParsedValue),
    jsonparse(['[' | NextValues], Object).


%%% jsonaccess/3 jsonaccess(Jsonobj, Fields, Result).
%%% vero quando Result è recuperabile seguendo la catena di campi presenti in Fields
%%% (una lista) a partire da Jsonobj. Un campo rappresentato da N (con N un numero maggiore o
%%% uguale a 0) corrisponde a un indice di un array JSON.
jsonaccess("", [], []).
jsonaccess([], [], []).

jsonaccess("", [], _) :- 
    jsonaccess("", [], []).

jsonaccess(_, [], []) :-
    !.

jsonaccess(JSONString, Fields, Result) :-
    (
        string(JSONString);
        atom(JSONString)
    ),
    jsonparse(JSONString, Object),
    jsonaccess(Object, Fields, Result).

jsonaccess([jsonobj, [Attr, Value] | _], Attr, Value) :-
    string(Attr).

jsonaccess([jsonobj, [_, _] | Members], Field, Result) :-
    string(Field),
    nth0(_, Members, [Field, Result]).

jsonaccess([jsonobj, [Attr, Value] | _], [Attr], [Value]) :-
    string(Attr),
    !.

jsonaccess([jsonobj, [_, _] | Members], [Field], [Result]) :-
    string(Field),
    nth0(_, Members, [Field, Result]),
    !.

jsonaccess([jsonarray, Value | Elements], [Index], Result) :-
    integer(Index),
    length([Value | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Value | Elements], Result).

jsonaccess([jsonobj, [Attr, Value] | Members], [Attr | Fields], [Value | Result]) :-
    string(Attr),
    jsonaccess([[Attr, Value] | Members], Fields, Result).    

jsonaccess([jsonobj, [_, _] | Members], [Attr | Fields], [Elem | Results]) :-
    string(Attr),
    nth0(_,[[_, _] | Members],[Attr, Elem]),
    jsonaccess([[_, _] | Members], Fields, Results). 

jsonaccess([jsonarray, Value | Elements], Index, Result) :-
    integer(Index),
    length([Value | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Value | Elements], Result).

jsonaccess([jsonarray, Value | Elements], [Index | Indexes], [Elem | Result]) :-
    integer(Index),
    length([Value | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Value | Elements], Elem),
    jsonaccess([Value | Elements], Indexes, Result).  

%%% jsonread/2 jsonread(FileName, JSON).
%%% legge da un file una stringa json

jsonread("",[]).
jsonread('',[]).

jsonread(FileName, JSON) :-
    read_file_to_string(FileName, String, []),
    jsonparse(String, JSON).

%%% jsondump/2 jsondump(JSON, FileName).
%%% scrive in un file una stringa json

jsondump("", "").
jsondump('','').

jsondump(JSON, FileName) :-
    \+(is_list(JSON)),
    jsonparse(JSON, _),
    open(FileName, write, Out, [create([write])]),
    write(Out, JSON),
    close(Out).

jsondump([_ | JSON], FileName) :-
    jsonparse(MetaString, JSON),
    addquotes(MetaString, TString),
    atomics_to_string(TString, '\s', String),
    open(FileName, write, Out, [create([write])]),
    write(Out, String),
    close(Out).

    

    

