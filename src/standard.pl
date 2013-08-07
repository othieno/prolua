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

type(niltype(_), 'Nil').
type(booleantype(_), 'Boolean').
type(numbertype(_), 'Number').
type(stringtype(_), 'String').
type(tabletype(_), 'Table').
type(functiontype(_, _, _), 'Function').
type(referencetype(_), 'Reference').




% Environment manipulation.
% ------------------------------------------------------------------------------

% Add a new empty table to the environment.
env_make(ETS, RS, ETS1, RS1) :-
   append(ETS, [tabletype([])], ETS1),
   list_size(ETS1, Size),
   append([referencetype(Size)], RS, RS1).

% Add a table with the given fields to the environment.
env_make(ETS, RS, Fields, ETS1, RS1) :-
   append(ETS, [tabletype(Fields)], ETS1),
   list_size(ETS1, Size),
   append([referencetype(Size)], RS, RS1).

% Add a table with the key-value pairs to the environment.
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
env_setvalue([E | ES], referencetype(1), K, V, [NE | ES]) :-
   table_set(E, K, V, NE).
env_setvalue([E | ES], referencetype(N), K, V, [E | NES]) :-
   referencetype(N),
   M is N - 1,
   env_setvalue(ES, referencetype(M), K, V, NES).



% List manipulation.
% ------------------------------------------------------------------------------

% Return the number of elements in a list.
list_size([], 0).
list_size([_ | Sublist], Size) :-
   list_size(Sublist, SublistSize),
   Size is SublistSize + 1.




% Table manipulation.
% ------------------------------------------------------------------------------

% Return the number of elements in a table.
table_size(tabletype(Fields), Size) :- list_size(Fields, Size).



% Create a table from two lists of keys and values.
table_create([], _, tabletype([])).
table_create([Key | Keys], [Value | Values], tabletype(Fields)) :-
   table_create(Keys, Values, tabletype(T)),
   append([[Key, Value]], T, Fields).
table_create([Key | Keys], [], tabletype(Fields)) :-
   table_create(Keys, [], tabletype(T)),
   append([[Key, niltype(nil)]], T, Fields).



% Get a field value based on its key.
table_get(tabletype([]), _, niltype(nil)).
table_get(tabletype([[K, V] | _]),  K,  V).
table_get(tabletype([[K, _] | T]), K1, V1) :-
   K \= K1,
   table_get(tabletype(T), K1, V1).



% Return true if a field key exists, false otherwise.
table_keyexists(tabletype([]), _, booleantype(false)).
table_keyexists(tabletype([[K, _] | _]),  K, booleantype(true)).
table_keyexists(tabletype([[K, _] | T]), K1, Exists) :-
   K \= K1,
   table_keyexists(tabletype(T), K1, Exists).



% Set the value of a field in the table.
table_set(tabletype([]), Key, Value, tabletype([[Key, Value]])).
table_set(tabletype([[K, _] | T]),  K,  V, tabletype([[K, V] |  T])).
table_set(tabletype([[K, V] | T]), K1, V1, tabletype([[K, V] | T1])) :-
   K1 \= K,
   table_set(tabletype(T), K1, V1, tabletype(T1)).



% Feedback predicates.
% ------------------------------------------------------------------------------

% Print the call stack.
printCallStack([]).
printCallStack(Statements) :-
   write('Call stack:\n'),
   printCallStack(Statements, 1).

printCallStack([], _).
printCallStack([Statement | Statements], InstructionNumber) :-
   write(InstructionNumber), write(' :: '), write(Statement), nl,
   NextInstructionNumber is InstructionNumber + 1,
   printCallStack(Statements, NextInstructionNumber).


% Print the result of an execution.
printResult([]) :-
   write('% No results were returned.\n').

printResult(error(ErrorMessage)) :-
   write('% Error! '), write(ErrorMessage), nl.

printResult(Result) :-
   write('% Result: '), write(Result), nl.


% Print the environment.
printEnvironment(Environment) :-
   write('% Environment: '), write(Environment), nl.



% Print execution statistics.
printStatistics :-
   statistics(runtime, [CPUTime | _]),
   Runtime is CPUTime/1000,
   write('% Evaluated in '), write(Runtime), write(' seconds.'), nl.
