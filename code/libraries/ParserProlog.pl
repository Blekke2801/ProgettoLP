%%% verifica oggetto o array prolog
jsonobj([[Attr, Value] | Members]) :-
    string(Attr),
    (
        is_list(Value);
        string(Value);
        number(Value)
    ),
    jsonobj(Members).

jsonarray([Value | Elements]) :-
    (
        is_list(Value);
        string(Value);
        number(Value)
    ),
    jsonarray(Elements).

%%% trim elimina tot elementi allinizio di una lista utlizzando l'append
trim(L,N,S) :-    
    length(P,N),   
    append(P,S,L).

%%% subobj/3 subobj([A | List], N, [A | Object])
%%% prendendo in input una lista con agli estremi delle parentesi graffe, crea una lista contenente un sotto-oggetto
subobj([],0,[]).
subobj([],_,[]).

subobj(['{' | List], N, ['{' | Object]) :-
    N1 is N + 1,
    !,
    subobj(List, N1, Object).

subobj(['}' | List], N, ['}' | Object]) :-
    N1 is N - 1,
    !,
    subobj(List, N1, Object).

subobj([A | List], N, [A | Object]) :-
    N > 0,
    !,
    subobj(List, N, Object).

subobj([A | _], 0, _) :-
    \+(A == '{'),
    \+(A == '}'),  
    !.


%%% subarray/3 subarray([A | List], N, [A | Object])
%%% subarray in input una lista con agli estremi delle parentesi quadrate, crea una lista contenente un sotto-array

subarray(['[' | List], N, ['[' | Object]) :-
    N1 is N + 1,
    !,
    subarray(List, N1, Object).

subarray([']' | List], N, [']' | Object]) :-
    N1 is N - 1,
    !,
    subarray(List, N1, Object).

subarray([A | List], N, [A | Object]) :-
    N > 0,
    !,
    subarray(List, N, Object).

subarray([A | _], 0, _) :-
    \+(A == '['),
    \+(A == ']'),  
    !.


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

unifyquotes(['\"' | _],_) :-
    unifyquotes([], []).

unifyquotes([A | As], [A | Ns]) :-
    unifyquotes(As,  Ns).    

unifynumbers([A | As], [A | Ns]) :-
    atom_number(A,_),
    !,
    unifynumbers(As,  Ns).

unifynumbers(['.' | As], ['.' | Ns]) :-
    unifynumbers(As,  Ns).

unifynumbers(_, _) :-
    unifynumbers([], []).

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
    atom_chars(JSONString, Chars),
    unify(Chars, Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', Xf),
    jsonparse(Xf, Object).

jsonparse(JSONString, Object) :-
    string(JSONString),
    string_chars(JSONString, Chars),
    unify(Chars,Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', Xf),
    jsonparse(Xf, Object).

jsonparse(['{', Attr, ':', Value , '}'], [[Attr, Value]]) :-
    string(Attr),
    (
        number(Value);
        string(Value)
    ),
    jsonparse(['{','}'], _).

jsonparse(['[', Value, ']'], [Value]) :-
    (
        number(Value);
        string(Value)
    ),
    jsonparse(['[',']'], _).

jsonparse(['{', Attr, ':', Value , ',' | Members], [[Attr, Value] | Object]) :-
    string(Attr),
    (
        string(Value);
        number(Value)
    ),
    jsonparse(['{' | Members], Object).


jsonparse(['{', Attr, ':', '{' , D | Members], [[Attr, ParsedValue] | Object]) :-
    string(Attr),
    \+(D == '}'),
    !,
    subobj([D | Members], 1, TrueValue),
    length(TrueValue, N),
    N1 is N + 3,
    trim(['{', Attr, ':', '{' , D | Members], N1, NextMembers),
    jsonparse(['{' | TrueValue], ParsedValue),
    jsonparse(['{' | NextMembers], Object).

jsonparse(['{', Attr, ':', '{' , '}' | Members], [[Attr, []] | Object]) :-
    string(Attr),
    jsonparse(['{' | Members], Object).

jsonparse(['[', Value, ',' | MoreValues], [Value | Objects]) :-
    (
        string(Value);
        number(Value)
    ),
    jsonparse(['[' | MoreValues], Objects).

jsonparse(['[', '{', D | MoreValues], [ParsedValue | Object]) :-
    \+(D == '}'),
    !,
    subobj([D | MoreValues], 1, TrueValue),
    length(TrueValue, N),
    N1 is N + 1,
    trim(['[', '{', D | MoreValues], N1, NextValues),
    jsonparse(['{' | TrueValue], ParsedValue),
    jsonparse(['[' | NextValues], Object).

jsonparse(['[', '{', '}' | MoreValues], [[] | Object]) :-
    jsonparse(['[' | MoreValues], Object).

jsonparse(['[', '[', D | MoreValues], [ParsedValue | Object]) :-
    \+(D == ']'),
    !,
    subarray([D | MoreValues], 1, TrueValue),
    length(TrueValue, N),
    N1 is N + 1,
    trim(['[', '[', D | MoreValues], N1, NextValues),
    jsonparse(['[' | TrueValue], ParsedValue),
    jsonparse(['[' | NextValues], Object).

jsonparse(['[', '[', ']' | MoreValues], [[] | Object]) :-
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
    jsonaccess(Members, Field, Result).

jsonaccess([[Attr, Value] | Members], [Attr | Fields], [Value | Result]) :-
    string(Attr),
    jsonaccess(Members, Fields, Result).    

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
    jsonparse(JSON, _),
    open(FileName, write, Out, [create([write])]),
    write(Out, JSON),
    close(Out).
    

