/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013 Jeremy Othieno.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */



% The standard library provides functions that help with tasks in the interpreter.
% Many of these functions are not documented in the specification but the code is
% simple to understand.
% ------------------------------------------------------------------------------

% Convert a decimal to hexadecimal.
hex(Decimal, Hexadecimal) :-
   number(Decimal),
   format(atom(Hexadecimal), '0x~16R', [Decimal]), !.
hex(Decimal, Decimal) :- \+number(Decimal).



% Format values to ease readability.
value_format(type(T), T) :- !.
value_format(niltype(nil), nil) :- !.
value_format(numbertype(N), Output) :-
   !,
   format(atom(Output), '~g', [N]).
value_format(booleantype(B), B) :- !.
value_format(stringtype(S), Output) :- !, format(atom(Output), '"~w"', S).
value_format(referencetype(Type, Address), Output) :-
   !,
   format(atom(Output), '~w:~w', [Type, Address]).

value_format(function(PS, _, path(Indices)), Output) :-
   !,
   value_format(PS, FPS),
   format(atom(Output), '<function(~w), ~w>', [FPS, Indices]).

value_format(table(FS, MT), Output) :-
   !,
   value_format(map(FS), FFS),
   (
      MT = referencetype(_, Address) ->
      format(atom(Output), '{~w} | metatable: ~w', [FFS, Address]);
      format(atom(Output), '{~w}', [FFS])
   ).

value_format([], '') :- !.
value_format([Value], Output) :-
   !,
   value_format(Value, Output).
value_format([Value | Values], Output) :-
   !,
   value_format(Value, TMP0),
   value_format(Values, TMP1),
   format(atom(Output), '~w, ~w', [TMP0, TMP1]).

value_format(map([]), '') :- !.
value_format(map([[K, V]]), Output) :-
   !,
   value_format(K, FK),
   value_format(V, FV),
   format(atom(Output), '[~w] = ~w', [FK, FV]), !.
value_format(map([[K, V] | VS]), Output) :-
   !,
   value_format(K, FK),
   value_format(V, FV),
   value_format(map(VS), FVS),
   format(atom(Output), '[~w] = ~w, ~w', [FK, FV, FVS]).

value_format(graph(Graph), Output) :-
   !,
   dag_countNodes(graph(Graph), NumberOfNodes),
   (
      NumberOfNodes > 1 ->
      format(atom(Output), 'Execution context graph with ~w nodes.', [NumberOfNodes]);
      (
         NumberOfNodes = 1 ->
         format(atom(Output), 'Execution context graph with 1 node.', []);
         format(atom(Output), 'Empty execution context graph.', [])
      )
   ).

value_format(Input, Input) :- !.





% Output.
% ------------------------------------------------------------------------------

% Print a line.
writeln(Line) :- write(Line), nl.


% Print a stack.
print_stack(path([]), _).
print_stack(path(Indices), Pool) :-
(
   Indices = [] ->
   true;
   (
      getContext(path(Indices), Pool, Context),
      length(Indices, Length),
      ContextIdentifier is Length - 1,
      print_context(Context, ContextIdentifier), !,
      popContext(path(Indices), NewPath),
      print_stack(NewPath, Pool)
   )
).


% Print a memory pool.
print_pool(pool(Offset, Memory)) :-
(
   Memory = [] ->
   (
      hex(Offset, Address),
      ansi_format([fg(yellow)], '   ~w ', [Address]),
      ansi_format([bold], '←\n', [])
   );
   (
      [[Address, References, Object] | MemoryBlocks] = Memory,
      ansi_format([bold, fg(yellow)], '   ~w', [Address]),
      ansi_format([bold, fg(blue)], '~t ~w ~t', [References]),
      value_format(Object, FormattedObject),
      ansi_format([], ' ~w~n', [FormattedObject]),
      print_pool(pool(Offset, MemoryBlocks))
   )
).


% Print an execution context.
print_context(context(Map, Lifetime), ECID) :-
   ansi_format([bold, negative, fg(magenta)], '   Execution Context ~w (~w)\n', [ECID, Lifetime]),
   print_map(Map, '   ').


% Print a map.
print_map([], _).
print_map([[Key, Value] | Map], Whitespace) :-
   ansi_format([bold, fg(yellow)], '~w~w', [Whitespace, Key]),
   value_format(Value, FormattedValue),
   ansi_format([], ' ~w\n', [FormattedValue]),
   print_map(Map, Whitespace).


% Print the call stack (the statements to be executed).
print_statements([]).
print_statements(Statements) :-
   writeln('Call stack:'),
   print_statements(Statements, 1).
print_statements([], _).
print_statements([Statement | Statements], InstructionNumber) :-
   write(InstructionNumber), write(' :: '), write(Statement), nl,
   NextInstructionNumber is InstructionNumber + 1,
   print_statements(Statements, NextInstructionNumber).


% Print the result of an execution, formatted for readability.
print_result(error(Message)) :-
   ansi_format([bold, fg(red)], '% Error! ~w\n', [Message]).
print_result([]) :-
   ansi_format([bold, fg(green)], '% No results were returned.\n', []).
print_result(Result) :-
   value_format(Result, FormattedResult),
   ansi_format([bold, fg(green)], '% ~w\n', [FormattedResult]).


% Print the execution environment.
print_environment([path([]), pool(_, [])]) :-
   ansi_format([bold, fg(magenta)], '% Empty execution environment.\n', []), !.
print_environment([Path, Pool]) :-
   (
      path(Indices) = Path,
      Indices = [] ->
      (
         ansi_format([bold], ' · EMPTY STACK~n', [])
      );
      (
         print_stack(Path, Pool),
         ansi_format([bold], ' ↑ STACK~n', [])
      )
   ),
   (
      pool(_, MemoryBlocks) = Pool,
      MemoryBlocks = [] ->
      (
         ansi_format([bold], ' · EMPTY HEAP~n', [])
      );
      (
         ansi_format([bold], ' ↓ HEAP~n', []),
         print_pool(Pool)
      )
   ).



% Print execution statistics, formatted for readability.
print_statistics(Runtime) :-
   ansi_format([bold], '% Evaluated in ~3f seconds.\n', [Runtime]).
