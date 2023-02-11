%%% trim elimina tot elementi allinizio di una lista utlizzando l'append
trim(L,N,S) :-    
    length(P,N),   
    append(P,S,L).

%%% subobj/3 subobj([A | List], N, [A | Object])
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

%%% subarray/3 subarray([A | List], N, [A | Object])
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

%%% jsonparse/2 jsonparse(JSONString, Object).
%%% prima funzione, vera quando la stringa può essere scomposta in stringhe numeri o termini composti
%%% per distinguere array da oggetto mantenere le parentesi anche in object, forse, non lo so in realtà

%%% inizio scrittura di jsonparse
jsonparse("", [null]).
jsonparse([],[null]).
jsonparse(['{','}'],[jsonobj]).
jsonparse(['[',']'],[jsonarray]).

jsonparse(JSONString, [jsonobj | Object]) :-
    atom(JSONString),
    atom_chars(JSONString, Chars),
    unify(Chars, Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', Xf),
    jsonparse(Xf, Comp0),
    member(jsonobj, Comp0),
    \+(member(jsonarray, Comp0)),
    delete(Comp0, jsonobj, Object).

jsonparse(JSONString, [jsonarray | Object]) :-
    atom(JSONString),
    atom_chars(JSONString, Chars),
    unify(Chars, Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', Xf),
    jsonparse(Xf, Comp0),
    member(jsonarray, Comp0),
    \+(member(jsonobj, Comp0)),
    delete(Comp0, jsonarray, Object).

jsonparse(JSONString, [jsonobj | Object]) :-
    string(JSONString),
    string_chars(JSONString, Chars),
    unify(Chars,Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', Xf),
    jsonparse(Xf, Comp0),
    member(jsonobj, Comp0),
    \+(member(jsonarray, Comp0)),
    delete(Comp0, jsonobj, Object).

jsonparse(JSONString, [jsonarray | Object]) :-
    string(JSONString),
    string_chars(JSONString, Chars),
    unify(Chars,Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', Xf),
    jsonparse(Xf, Comp0),
    member(jsonarray, Comp0),
    \+(member(jsonobj, Comp0)),
    delete(Comp0, jsonarray, Object).

jsonparse(['{', Attr, ':', Value , '}'], [jsonobj, [Attr, Value]]) :-
    string(Attr),
    (
        number(Value);
        string(Value)
    ),
    jsonparse(['{','}'], _).

jsonparse(['[', Value, ']'], [jsonarray, Value]) :-
    (
        number(Value);
        string(Value)
    ),
    jsonparse(['[',']'], _).

jsonparse(['{', Attr, ':', Value , ',' | Members], [jsonobj, [Attr, Value] | Object]) :-
    string(Attr),
    (
        string(Value);
        number(Value)
    ),
    jsonparse(['{' | Members], Object).

jsonparse(['{', Attr, ':', '{' , D | Members], [jsonobj, [Attr, [jsonobj | ParsedValue]] | Object]) :-
    string(Attr),
    \+(D == '}'),
    !,
    subobj([D | Members], TrueValue),
    length(TrueValue, N),
    N1 is N + 4,
    trim(['{', Attr, ':', '{' , D | Members], N1, NextMembers),
    jsonparse(['{' | TrueValue], Comp0),
    member(jsonobj, Comp0),
    delete(Comp0, jsonobj, ParsedValue),
    jsonparse(['{' | NextMembers], Object).

jsonparse(['{', Attr, ':', '{' , '}' | Members], [jsonobj, [Attr, [jsonobj]] | Object]) :-
    string(Attr),
    jsonparse(['{' | Members], Object).

jsonparse(['{', Attr, ':', '[' , D | Members], [jsonobj, [Attr, [jsonarray | ParsedValue]] | Object]) :-
    string(Attr),
    \+(D == '}'),
    !,
    subarray([D | Members], TrueValue),
    length(TrueValue, N),
    N1 is N + 4,
    trim(['{', Attr, ':', '[' , D | Members], N1, NextMembers),
    jsonparse(['[' | TrueValue], Comp0),
    member(jsonarray, Comp0),
    delete(Comp0, jsonarray, ParsedValue),
    jsonparse(['{' | NextMembers], Object).


jsonparse(['{', Attr, ':', '[' , ']' | Members], [jsonobj, [Attr, [jsonarray]] | Object]) :-
    string(Attr),
    jsonparse(['{' | Members], Object).

jsonparse(['[', Value, ',' | MoreValues], [jsonarray, Value | Objects]) :-
    (
        string(Value);
        number(Value)
    ),
    jsonparse(['[' | MoreValues], Objects).

jsonparse(['[', '{', D | MoreValues], [jsonarray, [jsonobj | ParsedValue] | Object]) :-
    \+(D == '}'),
    !,
    subobj([D | MoreValues], TrueValue),
    length(TrueValue, N),
    N1 is N + 2,
    trim(['[', '{', D | MoreValues], N1, NextValues),
    jsonparse(['{' | TrueValue], Comp0),
    member(jsonobj, Comp0),
    delete(Comp0, jsonobj, ParsedValue),
    jsonparse(['[' | NextValues], Object).

jsonparse(['[', '[', D | MoreValues], [jsonarray, [jsonarray | ParsedValue] | Object]) :-
    \+(D == ']'),
    !,
    subarray([D | MoreValues], TrueValue),
    length(TrueValue, N),
    N1 is N + 2,
    trim(['[', '[', D | MoreValues], N1, NextValues),
    jsonparse('[' | TrueValue, Comp0),
    member(jsonarray, Comp0),
    delete(Comp0, jsonarray, ParsedValue),
    jsonparse(['[' | NextValues], Object).

jsonparse(['[', '{', '}' | MoreValues], [[jsonobj] | Object]) :-
    jsonparse(['[' | MoreValues], Object).

jsonparse(['[', '[', ']' | MoreValues], [jsonarray, [jsonarray] | Object]) :-
    jsonparse(['[' | MoreValues], Object).

%%% jsonaccess/3 jsonaccess(Jsonobj, Fields, Result).
%%% vero quando Result è recuperabile seguendo la catena di campi presenti in Fields
%%% (una lista) a partire da Jsonobj. Un campo rappresentato da N (con N un numero maggiore o
%%% uguale a 0) corrisponde a un indice di un array JSON.

jsonaccess("", [], []).
jsonaccess([], [], []).

jsonaccess("", [], _) :- 
    jsonaccess("", [], []).

jsonaccess(_, [], _) :-
    !.

jsonaccess(JSONString, Fields, Result) :-
    (
        string(JSONString);
        atom(JSONString)
    ),
    jsonparse(JSONString, Object),
    jsonaccess(Object, Fields, Result).

jsonaccess([[Attr, Value] | _], Attr, Value) :-
    string(Attr).

jsonaccess([[_, _] | Members], Field, Result) :-
    string(Field),
    nth0(_, Members, [Field, Result]).

jsonaccess([[Attr, Value] | _], [Attr], [Value]) :-
    string(Attr),
    !.

jsonaccess([[_, _] | Members], [Field], [Result]) :-
    string(Field),
    nth0(_, Members, [Field, Result]),
    !.

jsonaccess([Value | Elements], [Index], Result) :-
    integer(Index),
    length([Value | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Value | Elements], Result).

jsonaccess([[Attr, Value] | Members], [Attr | Fields], [Value | Result]) :-
    string(Attr),
    jsonaccess([[Attr, Value] | Members], Fields, Result).    

jsonaccess([[_, _] | Members], [Attr | Fields], [Elem | Results]) :-
    string(Attr),
    nth0(_,[[_, _] | Members],[Attr, Elem]),
    jsonaccess([[_, _] | Members], Fields, Results). 

jsonaccess([Value | Elements], Index, Result) :-
    integer(Index),
    length([Value | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Value | Elements], Result).

jsonaccess([Value | Elements], [Index | Indexes], [Elem | Result]) :-
    integer(Index),
    length([Value | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Value | Elements], Elem),
    jsonaccess([Value | Elements], Indexes, Result).  

%%% jsonread/2 jsonread(FileName, JSON).
%%% legge da un file una stringa json

jsonread("",[]).

jsonread(FileName, JSON) :-
    read_file_to_string(FileName, String, []),
    jsonparse(String, JSON).

%%% jsondump/2 jsondump(JSON, FileName).
%%% scrive in un file una stringa json

jsondump("", "").

jsondump(JSON, FileName) :-
    \+(is_list(JSON)),
    !,
    jsonparse(JSON, _),
    open(FileName, write, Out, [create([write])]),
    write(Out, JSON),
    close(Out).

jsondump(JSON, FileName) :-
    jsonparse(MetaString, JSON),
    addquotes(MetaString, TString),
    atomics_to_string(TString, '\s', String),
    open(FileName, write, Out, [create([write])]),
    write(Out, String),
    close(Out).

addquotes([], []).

addquotes([A | List], [B | Result]) :-
    string(A),
    !,
    append([['\"'], [A],['\"']], Comp),
    atomics_to_string(Comp, B),
    addquotes(List, Result).

    addquotes([A | List], [A | Result]) :-
        addquotes(List, Result).
    

    

