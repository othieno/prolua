% The MIT License (MIT)
%
% Copyright (c) 2013 Jeremy Othieno.
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.


% The standard library provides functions that help with tasks in the interpreter.
% Many of these functions are not documented in the specification but the code is
% simple to understand.


% Type operator.
% ------------------------------------------------------------------------------

'std:type'(niltype(_), 'type:nil').
'std:type'(booleantype(_), 'type:boolean').
'std:type'(numbertype(_), 'type:number').
'std:type'(stringtype(_), 'type:string').
'std:type'(referencetype(table, _, _), 'type:table').
'std:type'(referencetype(function, _, _), 'type:function').



% The rawvalue predicate returns the actual value of an item excluding its type.
% ------------------------------------------------------------------------------

'std:rawValue'(niltype(nil), nil).
'std:rawValue'(booleantype(B), B).
'std:rawValue'(numbertype(N), N).
'std:rawValue'(stringtype(S), S).
'std:rawValue'(referencetype(_, _, R), R).
'std:rawValue'(functiontype(PS, SS, RS), functiontype(PS, SS, RS)).



% Map manipulation.
% ------------------------------------------------------------------------------

% Get a value in the map.
'map:getValue'([], _, niltype(nil)).
'map:getValue'([[K, V] | _], K, V).
'map:getValue'([[K, _] | M], K1, V) :-
   K \== K1,
   'map:getValue'(M, K1, V).

% Set a value in the map.
'map:setValue'([], K, V, [[K, V]]).
'map:setValue'([[K, _] | M], K, V, [[K, V] | M]).
'map:setValue'([[K, V] | M], K1, V1, [[K, V] | M1]) :-
   K \== K1,
   'map:setValue'(M, K1, V1, M1).

% Create a map from a list of keys and values. Be wary of varargs.
'map:build'([], _, []).
'map:build'(['...' | _], VS, [['...', VS]]).
'map:build'(['...' | _], [], [['...', [niltype(nil)]]]).
'map:build'([K | KS], [V | VS], M1) :-
   K \== '...',
   'map:build'(KS, VS, M),
   append([[K, V]], M, M1).
'map:build'([K | KS], [], M1) :-
   K \== '...',
   'map:build'(KS, [], M),
   append([[K, niltype(nil)]], M, M1).

% Does a given key exist in the map?
'map:keyExists'([], _) :- false.
'map:keyExists'([[K, _] | _], K).
'map:keyExists'([[K, _] | M], K1) :-
   K \== K1,
   'map:keyExists'(M, K1).

% Return the number of elements in the map.
'map:size'([], 0).
'map:size'([[_, _] | M], Size) :-
   'map:size'(M, SubmapSize),
   Size is SubmapSize + 1.

% Print a map.
'map:print'([], _).
'map:print'([[K, V] | M], Indentation) :-
   'std:whitespace'(Indentation, Whitespace),                                                         % TODO Use an asserta/1 here.
   write(Whitespace), write(K), write(': '), println(V),
   'map:print'(M, Indentation).



% isObject/2 returns true if the predicate is a Lua object, false otherwise.
% ------------------------------------------------------------------------------
'std:isObject'(table(_), table).
'std:isObject'(function(_, _, _), function).



% Environment manipulation.
% ------------------------------------------------------------------------------

'env:keyExists'(context(_, M), K) :-
   'map:keyExists'(M, K).
'env:keyExists'(ENV, ECID, _) :-
   'env:getContext'(ENV, ECID, error(_)),
   false.
'env:keyExists'(ENV, ECID, K) :-
   'env:getContext'(ENV, ECID, context(_, M)),
   'map:keyExists'(M, K).

'env:getIdentifier'(context(ECID, _), ECID).

'env:size'([], 0).
'env:size'([_ | ECS], Size) :-
   'env:size'(ECS, Subsize),
   Size is Subsize + 1.

'env:getContext'([], ECID, error(Message)) :-
   atom_concat('Could not find execution context #', ECID, Message).
'env:getContext'([context(ECID, M) | _], ECID, context(ECID, M)).
'env:getContext'([context(ECID, _) | ECS], ECID1, EC) :-
   ECID \== ECID1,
   'env:getContext'(ECS, ECID1, EC).

'env:getValue'(context(_, Map), Key, Value) :-
   'map:getValue'(Map, Key, Value).
'env:getValue'(ENV, ECID, Key, Value) :-
   'env:getContext'(ENV, ECID, context(_, Map)),
   'map:getValue'(Map, Key, Value).

'env:setValue'(context(ECID, M), K, V, context(ECID, M1)) :-
   \+'std:isObject'(V, _),
   'map:setValue'(M, K, V, M1).
'env:setValue'(context(ECID, M), K, V, context(ECID, M2)) :-
   'std:isObject'(V, ObjectType),
   'map:size'(M, ObjectKey),
   'map:setValue'(M, ObjectKey, V, M1),
   'map:setValue'(M1, K, referencetype(ObjectType, ECID, ObjectKey), M2).

'env:setValue'([], ECID, _, _, error(Message)) :-
   atom_concat('Could not find execution context #', ECID, Message).
'env:setValue'([context(ECID, M) | ECS], ECID, K, V, [context(ECID, M1) | ECS]) :-
   \+'std:isObject'(V, _),
   'map:setValue'(M, K, V, M1).
'env:setValue'([context(ECID, M) | ECS], ECID, K, V, [context(ECID, M2) | ECS]) :-
   'std:isObject'(V, ObjectType),
   'map:size'(M, ObjectKey),
   'map:setValue'(M, ObjectKey, V, M1),
   'map:setValue'(M1, K, referencetype(ObjectType, ECID, ObjectKey), M2).
'env:setValue'([context(ECID, _) | ECS], ECID1, K, V, error(Message)) :-
   ECID \== ECID1,
   'env:setValue'(ECS, ECID1, K, V, error(Message)).
'env:setValue'([context(ECID, M) | ECS], ECID1, K, V, ECS2) :-
   ECID \== ECID1,
   'env:setValue'(ECS, ECID1, K, V, ECS1),
   append([context(ECID, M)], ECS1, ECS2).

'env:addContext'([], [context(0, [])]).
'env:addContext'([context(ECID, M) | ECS], ENV) :-
   ECID1 is ECID + 1,
   append([context(ECID1, [])], [context(ECID, M) | ECS], ENV).
'env:addContext'(ENV0, KS, VS, [context(ECID, M) | ECS]) :-
   'env:addContext'(ENV0, [context(ECID, _) | ECS]),
   'map:build'(KS, VS, M).


'env:removeContext'([], _, []).
'env:removeContext'([context(ECID, _) | ECS], ECID, ECS).
'env:removeContext'([context(ECID, M) | ECS], ECID1, ECS2) :-
   ECID \== ECID1,
   'env:removeContext'(ECS, ECID1, ECS1),
   append([context(ECID, M)], ECS1, ECS2).


% The initial environment.
'env:initialise'(
   Statements,
   [
      context(0,
      [
         [0, function(['...'], Statements, [])],
         ['$prolua:main', referencetype(function, 0, 0)],
         [2, function(['value'], [return([unop(type, variable('value'))])], [])],
         ['type', referencetype(function, 0, 2)],
         [4, function(['output'], [], [])],
         ['print', referencetype(function, 0, 4)]
      ])
   ]
).



debug :-
   'env:initialise'([ok, ok1, ok2], ENV0),

   evaluate_rhs(ENV0, functioncall(variable('$prolua:main'), []), ENV1, R),
%   evaluate_rhs(ENV0, explist([numbertype(23), referencetype(function, 0), explist([booleantype(false), niltype(nil)])]), ENV1, R),
   'std:printfe'(ENV1),
   println(R).
   %'env:setvalue'(ENV0, 1, ce, function([], [], []), ENV1),
%   'std:printfe'(ENV1).










% Environment manipulation.
% ------------------------------------------------------------------------------

% Add a new empty table to the environment.
env_make(ETS, RS, ETS1, RS1) :-
   append(ETS, [table([])], ETS1),
   list_size(ETS1, Size),
   append([referencetype(Size)], RS, RS1).

% Add a table with the given fields to the environment.
env_make(ETS, RS, Fields, ETS1, RS1) :-
   append(ETS, [table(Fields)], ETS1),
   list_size(ETS1, Size),
   append([referencetype(Size)], RS, RS1).

% Add a table with the key-value pairs to the environment.
env_make(ETS, RS, ['...' | _], [], ETS1, RS1) :-
   append(ETS, [table([['...', [niltype(nil)]]])], ETS1),
   list_size(ETS1, Size),
   append([referencetype(Size)], RS, RS1).

% Be wary of variadic expressions.
env_make(ETS, RS, ['...' | _], Values, ETS1, RS1) :-
   append(ETS, [table([['...', Values]])], ETS1),
   list_size(ETS1, Size),
   append([referencetype(Size)], RS, RS1).

env_make(ETS, RS, Keys, Values, ETS1, RS1) :-
   table_create(Keys, Values, Table),
   append(ETS, [Table], ETS1),
   list_size(ETS1, Size),
   append([referencetype(Size)], RS, RS1).



% Return a table from the environment based on it reference.
env_gettable([ET | _], referencetype(1), ET).
env_gettable([_ | ETS], referencetype(N), ET) :-
   referencetype(N),
   M is N - 1,
   env_gettable(ETS, referencetype(M), ET).



% Return true if a given field key exists in a referenced table, false otherwise.
env_keyexists(ETS, Reference, Key, Exists) :-
   env_gettable(ETS, Reference, ET),
   table_keyexists(ET, Key, Exists).



% Return a field value from a given table and key.
env_getvalue(ETS, Reference, Key, Value) :-
   env_gettable(ETS, Reference, ET),
   table_get(ET, Key, Value).



% Set a field value in a referenced table based on its key.
env_setvalue([ET | ETS], referencetype(1), K, V, [ET1 | ETS]) :-
   table_set(ET, K, V, ET1).
env_setvalue([ET | ETS], referencetype(N), K, V, [ET | ETS1]) :-
   referencetype(N),
   M is N - 1,
   env_setvalue(ETS, referencetype(M), K, V, ETS1).


% Set a field value in the current environment.
env_setvalues(ETS, [], _, ETS).

env_setvalues(ETS, [[R, K] | RKS], [], ETS2) :-
   env_setvalue(ETS, R, K, niltype(nil), ETS1),
   env_setvalues(ETS1, RKS, [], ETS2).

env_setvalues(ETS, [[R, K] | RKS], [V | VS], ETS2) :-
   env_setvalue(ETS, R, K, V, ETS1),
   env_setvalues(ETS1, RKS, VS, ETS2).





% Table manipulation.
% ------------------------------------------------------------------------------

% Tables (associative arrays).
table([]).
table([[Key, Value] | T]) :-
   expression(Key),
   Key \= niltype(nil),
   expression(Value),
   table(T).




% Feedback predicates.
% ------------------------------------------------------------------------------

% Print a line.
println(Line) :- write(Line), nl.


% Format values to ease readability.
'std:format'(type(T), T).
'std:format'(niltype(nil), nil).
'std:format'(numbertype(N), N).
'std:format'(booleantype(B), B).
'std:format'(stringtype(S), S2) :-
   atom_concat('"',  S, S1),
   atom_concat(S1, '"', S2).
'std:format'(referencetype(Type, ECID, N), Reference) :-
   atom_concat(Type, ':', TMP0),
   atom_concat(TMP0, ECID, TMP1),
   atom_concat(TMP1, ':', TMP2),
   atom_concat(TMP2, N, Reference).
'std:format'([Value], FormattedValue) :-
   'std:format'(Value, FormattedValue).
'std:format'([Value | Values], FullyFormattedValues) :-
   'std:format'(Value, FormattedValue),
   'std:format'(Values, FormattedValues),
   atom_concat(FormattedValue, ', ', TMP),
   atom_concat(TMP, FormattedValues, FullyFormattedValues).
'std:format'(Environment, Environment).                                                               %TODO

% Create whitespace.
'std:whitespace'(0, '').
'std:whitespace'(N, Whitespace) :-
   M is N - 1,
   'std:whitespace'(M, TMP),
   atom_concat(' ', TMP, Whitespace).


% Print the call stack (the statements to be executed).
'std:printfi'([]).
'std:printfi'(Statements) :-
   println('Call stack:'),
   'std:printfi'(Statements, 1).

'std:printfi'([], _).
'std:printfi'([Statement | Statements], InstructionNumber) :-
   write(InstructionNumber), write(' :: '), write(Statement), nl,
   NextInstructionNumber is InstructionNumber + 1,
   'std:printfi'(Statements, NextInstructionNumber).



% Print the result of an execution, formatted for readability.
'std:printfr'(error(Message)) :-
   write('% Error! '), println(Message).

'std:printfr'([]) :-
   println('% No results were returned.').

'std:printfr'(Result) :-
   'std:format'(Result, FormattedResult),
   atom_concat('% Result: ', FormattedResult, FullyFormattedResult),
   println(FullyFormattedResult).



% Print the environment, formatted for readability.
'std:printfe'([EC]) :-
   'std:printfec'(EC).
'std:printfe'([EC | ECS]) :-
   'std:printfec'(EC),
   'std:printfe'(ECS).

% Print execution contexts.
'std:printfec'(context(ECID, M)) :-
   write('Execution context '), write(ECID), println(':'),
   'map:print'(M, 3).

% Print execution statistics, formatted for readability.
'std:printfs' :-
   statistics(runtime, [CPUTime | _]),
   Runtime is CPUTime/1000,
   write('% Evaluated in '), write(Runtime), write(' seconds.'), nl.
