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
'std:type'(referencetype(_), 'type:table').




% The rawvalue predicate.
% ------------------------------------------------------------------------------

'std:rawvalue'(niltype(nil), nil).
'std:rawvalue'(booleantype(B), B).
'std:rawvalue'(numbertype(N), N).
'std:rawvalue'(stringtype(S), S).
'std:rawvalue'(referencetype(R), R).
'std:rawvalue'(functiontype(PS, SS, RS), functiontype(PS, SS, RS)).



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



% List manipulation.
% ------------------------------------------------------------------------------

% Return the number of elements in a list.
list_size([], 0).
list_size([_ | Sublist], Size) :-
   list_size(Sublist, SublistSize),
   Size is SublistSize + 1.



% Table manipulation.
% ------------------------------------------------------------------------------

% Tables (associative arrays).
table([]).
table([[Key, Value] | T]) :-
   expression(Key),
   Key \= niltype(nil),
   expression(Value),
   table(T).


% Return the number of elements in a table.
table_size(table(Fields), Size) :- list_size(Fields, Size).



% Create a table from two lists of keys and values.
table_create([], _, table([])).
table_create([Key | Keys], [Value | Values], table(Fields)) :-
   table_create(Keys, Values, table(T)),
   append([[Key, Value]], T, Fields).
table_create([Key | Keys], [], table(Fields)) :-
   table_create(Keys, [], table(T)),
   append([[Key, niltype(nil)]], T, Fields).



% Get a field value based on its key.
table_get(table([]), _, niltype(nil)).
table_get(table([[K, V] | _]),  K,  V).
table_get(table([[K, _] | T]), K1, V1) :-
   K \= K1,
   table_get(table(T), K1, V1).



% Return true if a field key exists, false otherwise.
table_keyexists(table([]), _, booleantype(false)).
table_keyexists(table([[K, _] | _]),  K, booleantype(true)).
table_keyexists(table([[K, _] | T]), K1, Exists) :-
   K \= K1,
   table_keyexists(table(T), K1, Exists).



% Set the value of a field in the table.
table_set(table([]), Key, Value, table([[Key, Value]])).
table_set(table([[K, _] | T]),  K,  V, table([[K, V] |  T])).
table_set(table([[K, V] | T]), K1, V1, table([[K, V] | T1])) :-
   K1 \= K,
   table_set(table(T), K1, V1, table(T1)).



% Feedback predicates.
% ------------------------------------------------------------------------------

% Print a line.
println(Line) :- write(Line), nl.


% Format values to ease readability.
'std:format'(type(T), T).
'std:format'(niltype(nil), 'nil').
'std:format'(numbertype(N), N).
'std:format'(booleantype(B), B).
'std:format'(stringtype(S), S2) :-
   atom_concat('"',  S, S1),
   atom_concat(S1, '"', S2).
'std:format'(functiontype(_, _, _), 'function*').
'std:format'(referencetype(N), Reference) :-
   atom_concat('table:', N, Reference).
'std:format'([Value], FormattedValue) :-
   'std:format'(Value, FormattedValue).
'std:format'([Value | Values], FullyFormattedValues) :-
   'std:format'(Value, FormattedValue),
   'std:format'(Values, FormattedValues),
   atom_concat(FormattedValue, ', ', TMP),
   atom_concat(TMP, FormattedValues, FullyFormattedValues).
'std:format'(Environment, Environment).                                                               %TODO



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
'std:printfe'(_).
'std:printfe'(Environment) :-
   println('Environment:'),
   'std:format'(Environment, FormattedEnvironment),
   println(FormattedEnvironment).



% Print execution statistics, formatted for readability.
'std:printfs' :-
   statistics(runtime, [CPUTime | _]),
   Runtime is CPUTime/1000,
   write('% Evaluated in '), write(Runtime), write(' seconds.'), nl.



% The initial environment table.
% ------------------------------------------------------------------------------
'std:et0'(table([
   [
      'type',
      functiontype(['value'], [return([unop(type, variable('value'))])], [])
   ]
])).
