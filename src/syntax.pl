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


% Sets of elements.
% ------------------------------------------------------------------------------

% List of expressions.
explist([]).
explist([Expression | Expressions]) :-
   expression(Expression),
   explist(Expressions).

% The name set and a list of name sets.
name(Name) :-
   atom(Name),
   Name \= '...'.

namelist([]).
namelist([Name | Names]) :-
   name(Name),
   namelist(Names).

% List of parameter names.
parname('...').
parname(Name) :- name(Name).

parlist([]).
parlist([ParameterName | ParameterNames]) :-
   parname(ParameterName),
   parlist(ParameterNames).

% List of variables.
varlist([]).
varlist([variable(Name) | Variables]) :-
   name(Name),
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

% A stack of table references.
referencestack([]).
referencestack([referencetype(N) | References]) :-
   referencetype(N),
   referencestack(References).


% Types and values. Values in Prolua are explicitly typed to help keep things
% clear, with the downside of having cumbersome operations.
% ------------------------------------------------------------------------------

% Nil data type.
niltype(nil).

% Booleans.
booleantype(false).
booleantype(true).

% Real numbers.
numbertype(N) :- number(N).

% Strings. Note that the string value is verified in lua2prolog.
stringtype(_).

% Table references.
referencetype(N) :-
   integer(N),
   N > 0.

% Function type.
functiontype(ParameterNames, Statements, References) :-
   parlist(ParameterNames),
   statementlist(Statements),
   referencestack(References).

% The value set.
value(niltype(nil)).
value(booleantype(B)) :- booleantype(B).
value(numbertype(N)) :- numbertype(N).
value(stringtype(S)) :- stringtype(S).
value(referencetype(N)) :- referencetype(N).
value(functiontype(PS, SS, RS)) :- functiontype(PS, SS, RS).



% Expressions.
% ------------------------------------------------------------------------------

% Table constructor.
tableconstructor(Fields) :- table(Fields).

% Enclosed expressions.
enclosed(Expression) :- expression(Expression).

% Variable expressions.
variable(Name) :- name(Name).

% Field accessor expression.
access(Reference, Key) :-
   expression(Reference),
   expression(Key).

% Unary operators.
unop(type, Expression) :- expression(Expression).
unop(unm,  Expression) :- expression(Expression).
unop(not,  Expression) :- expression(Expression).
unop(len,  Expression) :- expression(Expression).

% Binary operators.
binop(add, E1, E2) :- expression(E1), expression(E2).
binop(sub, E1, E2) :- expression(E1), expression(E2).
binop(mul, E1, E2) :- expression(E1), expression(E2).
binop(div, E1, E2) :- expression(E1), expression(E2).
binop(mod, E1, E2) :- expression(E1), expression(E2).
binop(pow, E1, E2) :- expression(E1), expression(E2).
binop( eq, E1, E2) :- expression(E1), expression(E2).
binop( lt, E1, E2) :- expression(E1), expression(E2).
binop( le, E1, E2) :- expression(E1), expression(E2).
binop( gt, E1, E2) :- expression(E1), expression(E2).
binop( ge, E1, E2) :- expression(E1), expression(E2).
binop(and, E1, E2) :- expression(E1), expression(E2).
binop( or, E1, E2) :- expression(E1), expression(E2).
binop(cat, E1, E2) :- expression(E1), expression(E2).

% Function definition.
function(PS, SS) :-
   parlist(PS),
   statementlist(SS).

% Function calls.
functioncall(E, ES) :-
   expression(E),
   explist(ES).

% The expression set.
expression(Expression) :- value(Expression).
expression(tableconstructor(FS)) :- tableconstructor(FS).
expression(enclosed(E)) :- enclosed(E).
expression(variable(Name)) :- variable(Name).
expression(access(R, K)) :- access(R, K).
expression('...').
expression(unop(N, E)) :- unop(N, E).
expression(binop(N, E1, E2)) :- binop(N, E1, E2).
expression(function(PS, SS)) :- function(PS, SS).
expression(functioncall(E, ES)) :- functioncall(E, ES).




% Statements.
% ------------------------------------------------------------------------------

% The assignment operation.
assign(LHS, RHS) :-
   explist(LHS),
   explist(RHS).

% The do operation.
do(Statements) :-
   statementlist(Statements).

% The while operation.
while(Expression, Statements) :-
   expression(Expression),
   statementlist(Statements).

% The repeat-until operation.
repeat(Expression, Statements) :-
   expression(Expression),
   statementlist(Statements).

% The if-else operation.
if(Expression, StatementTrue, StatementFalse) :-
   expression(Expression),
   statement(StatementTrue),
   statement(StatementFalse).

% Local variable declaration.
localvariable(Name, Value) :-
   name(Name),
   expression(Value).

% Return statement.
return(Expressions) :-
   explist(Expressions).


% The statement set.
statement(assign(LHS, RHS)) :- assign(LHS, RHS).
statement(functioncall(Expression, Expressions)) :- functioncall(Expression, Expressions).
statement(do(Statements)) :- do(Statements).
statement(while(Expression, Statements)) :- while(Expression, Statements).
statement(repeat(Expression, Statements)) :- repeat(Expression, Statements).
statement(if(E, S1, S2)) :- if(E, S1, S2).
statement(localvariable(Name, Value)) :- localvariable(Name, Value).
statement(return(Expressions)) :- return(Expressions).
statement(break).
