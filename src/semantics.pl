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


% This is the core of Prolua; where the evaluation semantics for all expressions
% and statements are defined.


% Expression evaluation.
% ----------------------








% Statement evaluation. (Temporary until the evaluation semantics has been completed. This
% just makes sure all input statements can be unified.)
% ---------------------
evaluate_s(Environment, _, Statement, continue, [], Environment) :-
   statement(Statement).








% Program evaluation. (Temporary until the evaluation semantics has been completed.)
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

