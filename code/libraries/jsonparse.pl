%%% inizio funzioni di utility

%%% addquotes/2 addquotes([A | List], [B | Res]) 
%%% aggiunge 2 doppi apici ai confini della stringa, questa funzione serve perché 
%%% atomics_to_string trasformando una lista in una stringa, le stringhe all'interno perdono 
%%% i doppi apici, che servono per la riconversione
addquotes([], []).

addquotes([A | List], [B | Res]) :-
    string(A),
    !,
    append([['\"'], [A],['\"']], Comp),
    atomics_to_string(Comp, B),
    addquotes(List, Res).

addquotes([A | List], [A | Res]) :-
        addquotes(List, Res).

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

subobj(['{' | List], [['{' | Sub] | Object]) :-
    subobj(List, Sub),
    flatten(Sub,Comp),
    length(Comp, N),
    trim(List,N, NewList),
    subobj(NewList, Object).

subobj(['[' | List], [['[' | Sub] | Object]) :-
    subarray(List, Sub),
    flatten(Sub,Comp),
    length(Comp, N),
    trim(List,N, NewList),
    subobj(NewList, Object).

subobj(['}' | _], ['}']) :-
    !.

subobj([A | List], [A | Object]) :-
    subobj(List, Object).

%%% subarray/2 subarray([A | List], [A | Object])
%%% subarray in input una lista con agli estremi delle parentesi quadrate, crea una lista contenente un sotto-array
subarray([], []).

subarray(['[' | List], [['[' | Sub] | Object]) :-
    subarray(List, Sub),
    flatten(Sub,Comp),
    length(Comp, N),
    trim(List,N, NewList),
    subarray(NewList, Object).

subarray(['{' | List], [['{' | Sub] | Object]) :-
    subobj(List, Sub),
    flatten(Sub,Comp),
    length(Comp, N),
    trim(List,N, NewList),
    subarray(NewList, Object).

subarray([']' | _], [']']) :-
    !.

subarray([A | List], [A | Object]) :-
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

jsonparse(JSONString, jsonobj(Object)) :-
    atom(JSONString),
    atom_chars(JSONString, Chars),
    unify(Chars, Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', [X | Xf]),
    X == '{',
    !,
    jsonparse([X | Xf], Object).

jsonparse(JSONString, jsonarray(Object)) :-
    atom(JSONString),
    !,
    atom_chars(JSONString, Chars),
    unify(Chars, Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', [X | Xf]),
    X == '[',
    jsonparse([X | Xf], Object).

jsonparse(JSONString, jsonobj(Object)) :-
    string(JSONString),
    string_chars(JSONString, Chars),
    unify(Chars,Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', [X | Xf]),
    X == '{',
    !,
    jsonparse([X | Xf], Object).

jsonparse(JSONString, jsonarray(Object)) :-
    string(JSONString),
    string_chars(JSONString, Chars),
    unify(Chars,Xt),
    delete(Xt, '\t', Xd),
    delete(Xd, '\s', Xe),
    delete(Xe, '\n', [X | Xf]),
    X == '[',
    jsonparse([X | Xf], Object).

jsonparse(['{', Attr, ':', Val , '}'], [(Attr, Val)]) :-
    string(Attr),
    (
        number(Val);
        string(Val)
    ),
    !.

jsonparse(['{', Attr, ':', ['{' | Val] , '}'], [(Attr, jsonobj(PVal))]) :-
    string(Attr),
    jsonparse(['{' | Val],PVal),
    !.

jsonparse(['{', Attr, ':', ['[' | Val] , '}'], [(Attr, jsonarray(PVal))]) :-
    string(Attr),
    jsonparse(['[' | Val], PVal),
    !.

jsonparse(['[', Val, ']'], [Val]) :-
    (
        number(Val);
        string(Val)
    ),
    !.

jsonparse(['[', ['{' | Val], ']'], [jsonobj(PVal)]) :-
    jsonparse(['{' | Val],PVal),
    !.

jsonparse(['[', ['[' | Val], ']'], [jsonarray(PVal)]) :-
    jsonparse(['[' | Val], PVal),
    !.    
    
jsonparse(['{', Attr, ':', Val , ',' | Members], [(Attr, Val) | Object]) :-
    string(Attr),
    (
        string(Val);
        number(Val)
    ),
    jsonparse(['{' | Members], Object).

jsonparse(['{', Attr, ':', ['{' | Val], ',' | Members], [(Attr, jsonobj(PVal)) | Objects]) :-
    string(Attr),
    jsonparse(['{' | Val],PVal),
    jsonparse(['{' | Members], Objects).

jsonparse(['{', Attr, ':', ['[' | Val], ',' | Members], [(Attr, jsonarray(PVal)) | Objects]) :-
    string(Attr),
    jsonparse(['[' | Val], PVal),
    jsonparse(['{' | Members], Objects).
    
jsonparse(['{', Attr, ':', '{' | Members], Object) :-
    string(Attr),
    subobj(Members, TrueVal),
    flatten(TrueVal,Comp),
    length(Comp, N),
    N1 is N + 4,
    trim(['{', Attr, ':', '{' | Members], N1, NextMembers),
    jsonparse(['{', Attr, ':', ['{' | TrueVal] | NextMembers], Object).

jsonparse(['{', Attr, ':', '[' | Members], Object) :-
    string(Attr),
    subarray(Members, TrueVal),
    flatten(TrueVal,Comp),
    length(Comp, N),
    N1 is N + 4,
    trim(['{', Attr, ':', '[' | Members], N1, NextMembers),
    jsonparse(['{', Attr, ':', ['[' | TrueVal] | NextMembers], Object).

jsonparse(['[', Val, ',' | Vals], [Val | Objects]) :-
    (
        string(Val);
        number(Val)
    ),
    !,
    jsonparse(['[' | Vals], Objects).

jsonparse(['[', ['{' | Val], ',' | Vals], [jsonobj(PVal) | Objects]) :-
    jsonparse(['{' | Val], PVal),
    jsonparse(['[' | Vals], Objects).

jsonparse(['[', ['[' | Val], ',' | Vals], [jsonarray(PVal) | Objects]) :-
    jsonparse(['[' | Val], PVal),
    jsonparse(['[' | Vals], Objects).

jsonparse(['[', '{' | Vals], Object) :-
    subobj(Vals, TrueVal),
    flatten(TrueVal,Comp),
    length(Comp, N),
    N1 is N + 2,
    trim(['[', '{', Vals], N1, NextVals),
    jsonparse(['[', ['{' | TrueVal] | NextVals], Object).

jsonparse(['[', '[' | Vals], Object) :-
    subarray(Vals, TrueVal),
    flatten(TrueVal,Comp),
    length(Comp, N),
    N1 is N + 2,
    trim(['[', '[' | Vals], N1, NextVals),
    jsonparse(['[', ['[' | TrueVal] | NextVals], Object).


%%% jsonaccess/3 jsonaccess(Jsonobj, Fields, Res).
%%% vero quando Res è recuperabile seguendo la catena di campi presenti in Fields
%%% (una lista) a partire da Jsonobj. Un campo rappresentato da N (con N un numero maggiore o
%%% uguale a 0) corrisponde a un indice di un array JSON.
jsonaccess("", [], []).
jsonaccess(jsonarray(_), [], []).
jsonaccess(jsonobj(_), [], []).

jsonaccess(jsonobj(Members), [], jsonobj(Members)) :-
    !.

jsonaccess("", [], _) :- 
    jsonaccess("", [], []).

jsonaccess(JSONString, Fields, Res) :-
    (
        string(JSONString);
        atom(JSONString)
    ),
    jsonparse(JSONString, Object),
    jsonaccess(Object, Fields, Res).

jsonaccess(jsonobj([(Attr, Val) | _]), Attr, Val) :-
    string(Attr).

jsonaccess(jsonobj(Members), Field, Res) :-
    string(Field),
    nth0(_, Members, (Field, Res)).

jsonaccess(jsonobj([(Attr, Val) | _]), [Attr], Val) :-
    string(Attr),
    !.

jsonaccess(jsonobj(Members), [Field], Res) :-
    string(Field),
    nth0(_, Members, (Field, Res)),
    !.

jsonaccess(jsonarray([Val | Elements]), [Index], Res) :-
    integer(Index),
    length([Val | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Val | Elements], Res).

jsonaccess(jsonobj([(Attr, Val) | Members]), [Attr | Fields], [Val | Res]) :-
    string(Attr),
    jsonaccess(jsonobj([(Attr, Val) | Members]), Fields, Res).    

jsonaccess(jsonobj(Members), [Attr | Fields], [Elem | Ress]) :-
    string(Attr),
    nth0(_,Members,[Attr, Elem]),
    jsonaccess(jsonobj(Members), Fields, Ress). 

jsonaccess(jsonarray([Val | Elements]), Index, Res) :-
    integer(Index),
    length([Val | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Val | Elements], Res).

jsonaccess(jsonarray([Val | Elements]), [Index | Indexes], [Elem | Res]) :-
    integer(Index),
    length([Val | Elements], Int),
    Index < Int,
    !,
    nth0(Index,[Val | Elements], Elem),
    jsonaccess(jsonarray([Val | Elements]), Indexes, Res).  

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

jsondump(jsonobj(JSON), FileName) :-
    jsonparse(MetaString, JSON),
    flatten(MetaString, L),
    addquotes(L, TString),
    atomics_to_string(TString, '\s', String),
    open(FileName, write, Out, [create([write])]),
    write(Out, String),
    close(Out).

jsondump(jsonarray(JSON), FileName) :-
    jsonparse(MetaString, JSON),
    flatten(MetaString, L),
    addquotes(L, TString),
    atomics_to_string(TString, '\s', String),
    open(FileName, write, Out, [create([write])]),
    write(Out, String),
    close(Out).

jsondump(JSON, FileName) :-
    (
        string(JSON);
        atom(JSON)    
    ),
    jsonparse(JSON, _),
    open(FileName, write, Out, [create([write])]),
    write(Out, JSON),
    close(Out).


    

    

