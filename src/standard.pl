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
'std:rawValue'(referencetype(_, ECID, K), [ECID, K]).



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
   'print:indentation'(Indentation, Whitespace),                                                         % TODO Use an asserta/1 here.
   write(Whitespace), write(K), write(': '), writeln(V),
   'map:print'(M, Indentation).



% Environment manipulation.
% ------------------------------------------------------------------------------

% The initial environment mapping.
'env:0'(ECID,
   [
      [1, function(['value'], [return([unop(type, variable('value'))])], [])],
      ['type', referencetype(function, ECID, 1)],
      [3, function(['output'], [], [])],
      ['print', referencetype(function, ECID, 3)]
   ]
).


% Check if a key exists in a given context.
'env:keyExists'(context(_, M), K) :-
   'map:keyExists'(M, K).
'env:keyExists'(ENV, ECID, _) :-
   'env:getContext'(ENV, ECID, error(_)),
   false.
'env:keyExists'(ENV, ECID, K) :-
   'env:getContext'(ENV, ECID, context(_, M)),
   'map:keyExists'(M, K).


% Return an execution context's identifier.
'env:getIdentifier'(context(ECID, _), ECID).


% Return the size of an execution environment.
'env:size'([], 0).
'env:size'([_ | ECS], Size) :-
   'env:size'(ECS, Subsize),
   Size is Subsize + 1.


% Return a context based on a context identifier.
'env:getContext'([], ECID, error(Message)) :-
   atom_concat('Could not find execution context #', ECID, Message).
'env:getContext'([context(ECID, M) | _], ECID, context(ECID, M)).
'env:getContext'([context(ECID, _) | ECS], ECID1, EC) :-
   ECID \== ECID1,
   'env:getContext'(ECS, ECID1, EC).

% Add an object to the execution context.
'env:addObject'(context(ECID, M), table(T), context(ECID, M1), Reference) :-
   'map:size'(M, ObjectKey),
   'map:setValue'(M, ObjectKey, table(T), M1),
   Reference = referencetype(table, ECID, ObjectKey).
'env:addObject'(context(ECID, M), function(PS, SS, ENVf), context(ECID, M1), Reference) :-
   'map:size'(M, ObjectKey),
   'map:setValue'(M, ObjectKey, function(PS, SS, ENVf), M1),
   Reference = referencetype(function, ECID, ObjectKey).


% Return a value based on a context and key.
'env:getValue'(context(_, Map), Key, Value) :-
   'map:getValue'(Map, Key, Value).
'env:getValue'(ENV, ECID, Key, Value) :-
   'env:getContext'(ENV, ECID, context(_, Map)),
   'map:getValue'(Map, Key, Value).


% Set a value in a given context, based on a key.
'env:setValue'(context(ECID, M), K, V, context(ECID, M1)) :-
   'map:setValue'(M, K, V, M1).
'env:setValue'([], ECID, _, _, error(Message)) :-
   atom_concat('Could not find execution context #', ECID, Message).
'env:setValue'([context(ECID, M) | ECS], ECID, K, V, [context(ECID, M1) | ECS]) :-
   'map:setValue'(M, K, V, M1).
'env:setValue'([context(ECID, _) | ECS], ECID1, K, V, error(Message)) :-
   ECID \== ECID1,
   'env:setValue'(ECS, ECID1, K, V, error(Message)).
'env:setValue'([context(ECID, M) | ECS], ECID1, K, V, ECS2) :-
   ECID \== ECID1,
   'env:setValue'(ECS, ECID1, K, V, ECS1),
   append([context(ECID, M)], ECS1, ECS2).


% Set multiple values.
'env:setValues'(ENV0, [], _, ENV0).
'env:setValues'(ENV0, [[ECID, K] | AS], [], ENV2) :-
   'env:setValue'(ENV0, ECID, K, niltype(nil), ENV1),
   'env:setValues'(ENV1, AS, [], ENV2).
'env:setValues'(ENV0, [[ECID, K] | AS], [V | VS], ENV2) :-
   V \= referencetype(_, _, _),
   'env:setValue'(ENV0, ECID, K, V, ENV1), !,
   'env:setValues'(ENV1, AS, VS, ENV2).
'env:setValues'(ENV0, [[ECID, K] | AS], [referencetype(Type, ECID, K1) | VS], ENV2) :-
   'env:setValue'(ENV0, ECID, K, referencetype(Type, ECID, K1), ENV1), !,
   'env:setValues'(ENV1, AS, VS, ENV2).
'env:setValues'(ENV0, [[ECID, K] | AS], [referencetype(Type, ECID1, K1) | VS], ENV3) :-
   ECID \== ECID1,
   'env:getValue'(ENV0, ECID1, K1, V),
   'env:getContext'(ENV0, ECID, context(_, M)),
   'map:size'(M, ObjectKey),
   'env:setValue'(ENV0, ECID, ObjectKey, V, ENV1),
   'env:setValue'(ENV1, ECID, K, referencetype(Type, ECID, ObjectKey), ENV2), !,
   'env:setValues'(ENV2, AS, VS, ENV3).


% Create a new context.
'env:addContext'([], [context(0, [])]).
'env:addContext'([context(ECID, M) | ECS], ENV) :-
   ECID1 is ECID + 1,
   append([context(ECID1, [])], [context(ECID, M) | ECS], ENV).
'env:addContext'(ENV0, KS, VS, [context(ECID, M) | ECS]) :-
   'env:addContext'(ENV0, [context(ECID, _) | ECS]),
   'map:build'(KS, VS, M).


% Remove a context.
'env:removeContext'([], _, []).
'env:removeContext'([context(ECID, _) | ECS], ECID, ECS).
'env:removeContext'([context(ECID, M) | ECS], ECID1, ECS2) :-
   ECID \== ECID1,
   'env:removeContext'(ECS, ECID1, ECS1),
   append([context(ECID, M)], ECS1, ECS2).



% Feedback predicates.
% ------------------------------------------------------------------------------

% Print a line.
writeln(Line) :- write(Line), nl.


% Format values to ease readability.
'print:format'(type(T), T).
'print:format'(niltype(nil), nil).
'print:format'(numbertype(N), N).
'print:format'(booleantype(B), B).
'print:format'(stringtype(S), S2) :-
   atom_concat('"',  S, S1),
   atom_concat(S1, '"', S2).
'print:format'(referencetype(Type, ECID, N), Reference) :-
   atom_concat(Type, ':', TMP0),
   atom_concat(TMP0, ECID, TMP1),
   atom_concat(TMP1, ':', TMP2),
   atom_concat(TMP2, N, Reference).
'print:format'([Value], FormattedValue) :-
   'print:format'(Value, FormattedValue).
'print:format'([Value | Values], FullyFormattedValues) :-
   'print:format'(Value, FormattedValue),
   'print:format'(Values, FormattedValues),
   atom_concat(FormattedValue, ', ', TMP),
   atom_concat(TMP, FormattedValues, FullyFormattedValues).


% Print indentation whitespace.
'print:indentation'(0, '').
'print:indentation'(Count, Whitespace) :-
   M is Count - 1,
   'print:indentation'(M, TMP),
   atom_concat('   ', TMP, Whitespace).


% Print the call stack (the statements to be executed).
'print:statements'([]).
'print:statements'(Statements) :-
   writeln('Call stack:'),
   'print:statements'(Statements, 1).
'print:statements'([], _).
'print:statements'([Statement | Statements], InstructionNumber) :-
   write(InstructionNumber), write(' :: '), write(Statement), nl,
   NextInstructionNumber is InstructionNumber + 1,
   'print:statements'(Statements, NextInstructionNumber).


% Print the result of an execution, formatted for readability.
'print:result'(error(Message)) :-
   write('% Error! '), writeln(Message).
'print:result'([]) :-
   writeln('% No results were returned.').
'print:result'(Result) :-
   'print:format'(Result, FormattedResult),
   atom_concat('% Result: ', FormattedResult, FullyFormattedResult),
   writeln(FullyFormattedResult).


% Print execution contexts.
'print:context'(context(ECID, M)) :-
   write('   Execution context '), write(ECID), writeln(':'),
   'map:print'(M, 2).
'print:context'([]).
'print:context'([EC | ECS]) :-
   'print:context'(EC), !,
   'print:context'(ECS).


% Print the execution environment.
'print:environment'(Environment) :-
   writeln('% Execution environment: '),
   'print:context'(Environment).


% Print execution statistics, formatted for readability.
'print:statistics' :-
   statistics(runtime, [CPUTime | _]),
   Runtime is CPUTime/1000,
   write('% Evaluated in '), write(Runtime), write(' seconds.'), nl.
