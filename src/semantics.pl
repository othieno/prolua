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


% This is the core of Prolua; where the semantics are defined.


% Sets of elements.
% -----------------

% List of expressions.
explist([]).
explist([Expression | Expressions]) :-
   expression(Expression),
   explist(Expressions).

% The name set and a list of name sets.
name(Name) :-
   atom(Name),
   Name \= vararg.

namelist([]).
namelist([Name | Names]) :-
   name(Name),
   namelist(Names).

% List of parameter names.
parname(vararg).
parname(Name) :- name(Name).

parlist([]).
parlist([ParameterName | ParameterNames]) :-
   parname(ParameterName),
   parlist(ParameterNames).

% List of variables.
varlist([]).
varlist([variable(Name) | Variables]) :-
   variable(Name),
   varlist(Variables).

% List of values.
valuelist([]).
valuelist([Value | Values]) :-
   value(Value),
   valuelist(Values).

% List of statements.
statementlist([]).
statementlist([Statement | Statements]) :-
   statement(Statement),
   statementlist(Statements).

% List of tables.
tablelist([]).
tablelist([tabletype(Table) | Tables]) :-
   tabletype(Table),
   tablelist(Tables).

% A list of table references. Note that there's only one list of table
% references and it must contain at least one reference!
referencelist([referencetype(N)]) :- referencetype(N).
referencelist([referencetype(N) | References]) :-
   referencetype(N),
   referencelist(References).



% Types and values. Values in Prolua are explicitly typed to help keep things clear.
% ----------------------------------------------------------------------------------

% Nil data type.
niltype(nil).

% Booleans.
booleantype(false).
booleantype(true).

% Numbers.
numbertype(N) :- number(N).

% Strings. Note that the string value is verified in lua2prolog.
stringtype(_).

% Tables (associative arrays).
tablekey(Key) :-
   expression(Key),
   Key \= niltype(nil).

tabletype([]).
tabletype([[Key, Value] | T]) :-
   tablekey(Key),
   expression(Value),
   tabletype(T).

% Table references.
referencetype(N) :-
   integer(N), N > 0.

% Function bodies.
functionbody(ParameterNames, Statements) :-
   parlist(ParameterNames),
   statementlist(Statements).

% The value set.
value(niltype(nil)).
value(booleantype(B)) :- booleantype(B).
value(numbertype(N)) :- numbertype(N).
value(stringtype(S)) :- stringtype(S).
value(tabletype(T)) :- tabletype(T).
value(functionbody(N, S)) :- functionbody(N, S).
value(referencetype(N)) :- referencetype(N).
value(valuelist(Values)) :- valuelist(Values).



% Expressions.
% ------------

% Variables are expressions that evaluate into values.
variable(N) :- name(N).

% Unary operators.
unop(negative, Expression) :- expression(Expression).
unop(not, Expression) :- expression(Expression).
unop(length, Expression) :- expression(Expression).

% Binary operators.
binop(add, E1, E2) :- expression(E1), expression(E2).
binop(subtract, E1, E2) :- expression(E1), expression(E2).
binop(multiply, E1, E2) :- expression(E1), expression(E2).
binop(divide, E1, E2) :- expression(E1), expression(E2).
binop(modulo, E1, E2) :- expression(E1), expression(E2).
binop(exponent, E1, E2) :- expression(E1), expression(E2).
binop(equal, E1, E2) :- expression(E1), expression(E2).
binop(lt, E1, E2) :- expression(E1), expression(E2).
binop(le, E1, E2) :- expression(E1), expression(E2).
binop(gt, E1, E2) :- expression(E1), expression(E2).
binop(ge, E1, E2) :- expression(E1), expression(E2).
binop(and, E1, E2) :- expression(E1), expression(E2).
binop(or, E1, E2) :- expression(E1), expression(E2).
binop(concatenate, E1, E2) :- expression(E1), expression(E2).

% The access operator.
access(E1, E2) :- expression(E1), expression(E2).

% Function calls.
functioncall(E, ES) :- expression(E), explist(ES).

% The expression set.
expression(vararg).
expression(E) :- value(E).
expression(variable(N)) :- variable(N).
expression(unop(N, E)) :- unop(N, E).
expression(binop(N, E1, E2)) :- binop(N, E1, E2).
expression(access(T, K)) :- access(T, K).
expression(functioncall(E, ES)) :- functioncall(E, ES).



% Statements.
% -----------

% The assignment operation.
assign(E1, E2) :- explist(E1), explist(E2).

% The do operation.
do(Statements) :- statementlist(Statements).

% The while operation.
while(Expression, Statement) :- expression(Expression), statement(Statement).

% The repeat-until operation.
repeat(Expression, Statement) :- expression(Expression), statement(Statement).

% The if-else operation.
if(E, S1, S2) :- expression(E), statement(S1), statement(S2).

% The numeric for loop.
for(N, I, E, INC, S) :-
   name(N),
   expression(I),
   expression(E),
   expression(INC),
   statement(S).

% The generic for loop.
for(NS, ES, S) :-
   namelist(NS),
   explist(ES),
   statement(S).

% Local variable declaration.
localvariable(N, V) :- name(N), expression(V).

% Return statement.
return(ES) :- explist(ES).


% The statement set.
statement(assign(E1, E2)) :- assign(E1, E2).
statement(functioncall(E, ES)) :- functioncall(E, ES).
statement(do(Statements)) :- do(Statements).
statement(while(Expression, Statements)) :- while(Expression, Statements).
statement(repeat(Expression, Statements)) :- repeat(Expression, Statements).
statement(if(E, S1, S2)) :- if(E, S1, S2).
statement(for(N, I, E, INC, S)) :- for(N, I, E, INC, S).
statement(for(NS, ES, SS)) :- for(NS, ES, SS).
statement(localvariable(N, V)) :- localvariable(N, V).
statement(return(ES)) :- return(ES).
statement(break).


% Expression evaluation.
% ----------------------




% Statement evaluation.
% ---------------------

evaluate_s(Environment, _, Statement, continue, [], Environment) :-
   statement(Statement).




% Program evaluation.
% -------------------

% If the environment isn't a list of tables, then we have an error.
evaluate_p(Environment, _, _, error('Invalid environment.'), Environment) :-
   \+tablelist(Environment), !.

% If the given execution stack isn't a list of references, then we have an error.
evaluate_p(Environment, References, _, error('Invalid reference stack.'), Environment) :-
   \+referencelist(References), !.

% Evaluate an empty program.
evaluate_p(Environment, _, [], [], Environment).

% If the control is 'continue', we continue program execution all the while discarding
% intermediate results.
evaluate_p(Environment, References, [Statement | Statements], Result, Environment2) :-
   evaluate_s(Environment, References, Statement, continue, _, Environment1),
   evaluate_p(Environment1, References, Statements, Result, Environment2).

% If an error was raised while evaluating a statement, exit and propagate the error.
evaluate_p(Environment, References, [Statement | _], Error, Environment1) :-
   evaluate_s(Environment, References, Statement, error, Error, Environment1).

% If the control is 'return', we exit the program and return the result.
evaluate_p(Environment, References, [Statement | _], Result, Environment1) :-
   evaluate_s(Environment, References, Statement, return, Result, Environment1).
