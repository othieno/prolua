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


% Sets of elements.
% ------------------------------------------------------------------------------

% List of expressions.
expressions([]).
expressions([Expression | Expressions]) :-
   expression(Expression),
   expressions(Expressions).

% The name set and a list of name sets.
name(Name) :-
   atom(Name),
   Name \= '...'.

names([]).
names([Name | Names]) :-
   name(Name), !,
   names(Names).

% List of parameter names.
parameter(Name) :-
   name(Name); Name = '...'.

parameters([]).
parameters([Parameter | Parameters]) :-
   parameter(Parameter), !,
   parameters(Parameters).

% List of variables.
variables([]).
variables([variable(Name) | Variables]) :-
   name(Name), !,
   variables(Variables).

% List of values.
values([]).
values([Value | Values]) :-
   value(Value), !,
   values(Values).

% List of statements.
statements([]).
statements([Statement | Statements]) :-
   statement(Statement), !,
   statements(Statements).


% Types and values. Values in Prolua are explicitly typed to help keep things
% clear, with the downside of having cumbersome operations.
% ------------------------------------------------------------------------------

% Nil data type.
niltype(nil).


% Booleans.
booleantype(false).
booleantype(true).


% Real numbers.
numbertype(N) :-
   number(N).


% Strings.
stringtype(S) :-
   atom(S).


% Tables.
field(Key, Value) :-
   (name(Key); expression(Key)),
   Key \= niltype(nil),
   expression(Value), !.

fields([]).
fields([[Key, Value] | Fields]) :-
   field(Key, Value),
   fields(Fields), !.

table(Fields, _) :-
   fields(Fields), !.


% Functions.
function(Parameters, Statements, Reference) :-
   parameters(Parameters),
   statements(Statements),
   member(Reference, [[], referencetype(table, _)]), !.


% Object references.
referencetype(Type, Address) :-
   member(Type, [table, function]),
   atom_number(Address, N),
   integer(N),
   N >= 0, !.


% The value set.
value(niltype(nil)).
value(booleantype(B)) :- booleantype(B).
value(numbertype(N)) :- numbertype(N).
value(stringtype(S)) :- stringtype(S).
value(referencetype(Type, Address)) :- referencetype(Type, Address).




% Expressions.
% ------------------------------------------------------------------------------

% Table constructor.
tableconstructor([]).
tableconstructor(fields(Fields)) :-
   fields(Fields), !.
tableconstructor(expressions(Expressions)) :-
   expressions(Expressions), !.


% Enclosed expressions.
enclosed(Expression) :-
   expression(Expression).


% Variable expressions.
variable(Name) :-
   name(Name).


% Field accessor expression.
access(Expression, Key) :-
   expression(Expression),
   expression(Key),
   Key \= niltype(nil).


% Unary operators.
unop(Operator, Expression) :-
   member(Operator, [unm, not, len]),
   expression(Expression).


% Binary operators.
binop(Operator, LHS, RHS) :-
   member(Operator, [add, sub, mul, div, mod, pow, eq, lt, le, and, or, concat]),
   expression(LHS),
   expression(RHS).


% Function definition.
functiondef(Parameters, Statements) :-
   parameters(Parameters),
   statements(Statements).


% Function calls.
functioncall(Expression, Expressions) :-
   expression(Expression),
   expressions(Expressions).


% The expression set.
expression(Expression) :- value(Expression), !.
expression(tableconstructor(Fields)) :- tableconstructor(Fields), !.
expression(enclosed(Expression)) :- enclosed(Expression), !.
expression(variable(Name)) :- variable(Name), !.
expression(access(Expression, Key)) :- access(Expression, Key), !.
expression('...') :- !.
expression(unop(Operator, Expression)) :- unop(Operator, Expression), !.
expression(binop(Operator, LHS, RHS)) :- binop(Operator, LHS, RHS), !.
expression(functiondef(Parameters, Statements)) :- functiondef(Parameters, Statements), !.
expression(functioncall(Expression, Expressions)) :- functioncall(Expression, Expressions), !.



% Statements.
% ------------------------------------------------------------------------------

% The assignment operation.
assign(LHS, RHS) :-
   expressions(LHS),
   expressions(RHS).


% The do operation.
do(Statements) :-
   statements(Statements).


% The while operation.
while(Expression, Statements) :-
   expression(Expression),
   statements(Statements).


% The repeat-until operation.
repeat(Expression, Statements) :-
   expression(Expression),
   statements(Statements).


% The if-else operation.
if(Expression, True, False) :-
   expression(Expression),
   statement(True),
   statement(False).


% Local variable declaration.
localvariable(Name) :-
   name(Name).


% Return statement.
return(Expressions) :-
   expressions(Expressions).


% The statement set.
statement(assign(LHS, RHS)) :- assign(LHS, RHS).
statement(functioncall(Expression, Expressions)) :- functioncall(Expression, Expressions).
statement(do(Statements)) :- do(Statements).
statement(while(Expression, Statements)) :- while(Expression, Statements).
statement(repeat(Expression, Statements)) :- repeat(Expression, Statements).
statement(if(Expression, True, False)) :- if(Expression, True, False).
statement(localvariable(Name)) :- localvariable(Name).
statement(return(Expressions)) :- return(Expressions).
statement(break).
