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
% ------------------------------------------------------------------------------

% Evaluate a list of left-hand side (LHS) expressions.
evaluate_lhs(ENV0, explist([]), ENV0, []).
evaluate_lhs(ENV0, explist([E | _]), ENV1, error(Message)) :-
   evaluate_lhs(ENV0, E, ENV1, error(Message)).
evaluate_lhs(ENV0, explist([E | ES]), ENV2, error(Message)) :-
   evaluate_lhs(ENV0, E, ENV1, _),
   evaluate_lhs(ENV1, explist(ES), ENV2, error(Message)).
evaluate_lhs(ENV0, explist([E | ES]), ENV2, Result) :-
   evaluate_lhs(ENV0, E, ENV1, [ECID, K]),
   evaluate_lhs(ENV1, explist(ES), ENV2, AS),
   append([[ECID, K]], AS, Result).



% Evaluate a left-hand side variable.
evaluate_lhs([EC], variable(N), [EC], [ECID, N]) :-
   'env:getIdentifier'(EC, ECID).
evaluate_lhs([EC | ECS], variable(N), [EC | ECS], [ECID, N]) :-
   'env:keyExists'(EC, N),
   'env:getIdentifier'(EC, ECID).
evaluate_lhs([EC | ECS], variable(N), [EC | ECS], [ECID, N]) :-
   \+'env:keyExists'(EC, N),
   evaluate_lhs(ECS, variable(N), ECS, [ECID, N]).





                                                                                                      /*
% Evaluate a left-hand side field accessor.
evaluate_lhs(ETS, RS, access(R, _), ETS1, error(Message)) :-
   evaluate_rhs(ETS, RS, R, ETS1, error(Message)).

evaluate_lhs(ETS, RS, access(R, K), ETS2, error(Message)) :-
   evaluate_rhs(ETS, RS, R, ETS1, _),
   evaluate_rhs(ETS1, RS, K, ETS2, error(Message)).

evaluate_lhs(ETS, RS, access(R, K), ETS2, [V_r, V_k]) :-
   evaluate_rhs(ETS, RS, R, ETS1, [V_r | _]),
   evaluate_rhs(ETS1, RS, K, ETS2, [V_k | _]).
                                                                                                      */


% Evaluate a list of right-hand side (RHS) expressions.
evaluate_rhs(ENV0, explist([]), ENV0, []).
evaluate_rhs(ENV0, explist([E | _]), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).
evaluate_rhs(ENV0, explist([E | ES]), ENV2, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, _),
   evaluate_rhs(ENV1, explist(ES), ENV2, error(Message)).
evaluate_rhs(ENV0, explist([E]), ENV1, VS) :-
   evaluate_rhs(ENV0, E, ENV1, VS).
evaluate_rhs(ENV0, explist([E | ES]), ENV2, [VE | VS]) :-
   ES \== [],
   evaluate_rhs(ENV0, E, ENV1, [VE | _]),
   evaluate_rhs(ENV1, explist(ES), ENV2, VS).



% Evaluate values.
evaluate_rhs(ENV0, niltype(nil), ENV0, [niltype(nil)]).
evaluate_rhs(ENV0, booleantype(false), ENV0, [booleantype(false)]).
evaluate_rhs(ENV0, booleantype(true), ENV0, [booleantype(true)]).
evaluate_rhs(ENV0, numbertype(N), ENV0, [numbertype(N)]).
evaluate_rhs(ENV0, stringtype(S), ENV0, [stringtype(S)]).
evaluate_rhs(ENV0, referencetype(table, T), ENV0, [referencetype(table, T)]).
evaluate_rhs(ENV0, referencetype(function, F), ENV0, [referencetype(function, F)]).



% Table constructor.
evaluate_rhs(ENV0, tableconstructor([]), ENV0, [table([])]).
                                                                                                      /*

evaluate_rhs(ETS, RS, tableconstructor(FS), ETS1, [R]) :-
   env_make(ETS, RS, FS, ETS1, [R | _]).
                                                                                                      */




% Enclosed expressions.
evaluate_rhs(ENV0, enclosed(E), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).
evaluate_rhs(ENV0, enclosed(E), ENV1, []) :-
   evaluate_rhs(ENV0, E, ENV1, []).
evaluate_rhs(ENV0, enclosed(E), ENV1, [V]) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]).



% Evaluate a right-hand side variable.
evaluate_rhs(ENV0, variable(N), ENV1, error(Message)) :-
   evaluate_lhs(ENV0, variable(N), ENV1, error(Message)).
evaluate_rhs(ENV0, variable(N), ENV1, [V]) :-
   evaluate_lhs(ENV0, variable(N), ENV1, [ECID, N]),
   'env:getValue'(ENV1, ECID, N, V).



                                                                                                            /*
% Evaluate a right-hand side field accessor.
evaluate_rhs(ETS, RS, access(R, K), ETS1, error(Message)) :-
   evaluate_lhs(ETS, RS, access(R, K), ETS1, error(Message)), !.

evaluate_rhs(ETS, RS, access(R, K), ETS1, [V]) :-
   evaluate_lhs(ETS, RS, access(R, K), ETS1, [R1, K1]),
   env_getvalue(ETS1, R1, K1, V).
                                                                                                            */



% Evaluate a variadic expression.
evaluate_rhs([EC], '...', [EC], error('\'...\' is not defined.')) :-
   \+'env:keyExists'(EC, '...').
evaluate_rhs([EC], '...', [EC], VS) :-
   'env:keyExists'(EC, '...'),
   'env:getValue'(EC, '...', VS).
evaluate_rhs([EC | ECS], '...', [EC | ECS], VS) :-
   'env:keyExists'(EC, '...'),
   'env:getValue'(EC, '...', VS).
evaluate_rhs([EC | ECS], '...', [EC | ECS], VS) :-
   \+'env:keyExists'(EC, '...'),
   evaluate_rhs(ECS, '...', ECS, VS).



% Unary operators.
evaluate_rhs(ENV0, unop(_, E), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



% The type operator.
evaluate_rhs(ENV0, unop(type, E), ENV1, [type(Type)]) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   'std:type'(V, Type).







                                                                                                            /*
% The unary minus operator.
evaluate_rhs(ETS, RS, unop(unm, E), ETS1, [numbertype(M)]) :-
   evaluate_rhs(ETS, RS, E, ETS1, [numbertype(N) | _]),
   M is -N.

evaluate_rhs(ETS, RS, unop(unm, E), ETS1, error('\'-\' operand is not a number.')) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   \+member(V, [numbertype(_)]).



% The not operator.
evaluate_rhs(ETS, RS, unop(not, E), ETS1, [booleantype(true)]) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]).

evaluate_rhs(ETS, RS, unop(not, E), ETS1, [booleantype(false)]) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]).



% The length operator.
evaluate_rhs(ETS, RS, unop(len, E), ETS1, [numbertype(Size)]) :-
   evaluate_rhs(ETS, RS, E, ETS1, [referencetype(N) | _]),
   env_gettable(ETS1, referencetype(N), T),
   table_size(T, Size).

evaluate_rhs(ETS, RS, unop(len, E), ETS1, [numbertype(Size)]) :-
   evaluate_rhs(ETS, RS, E, ETS1, [stringtype(S) | _]),
   atom_length(S, Size).

evaluate_rhs(ETS, RS, unop(len, E), ETS1, error('\'#\' operand is not a table or string.')) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   \+member(V, [referencetype(_), stringtype(_)]).



% Binary operators.
evaluate_rhs(ETS, RS, binop(_, E1, _), ETS1, error(Message)) :-
   evaluate_rhs(ETS, RS, E1, ETS1, error(Message)).

evaluate_rhs(ETS, RS, binop(_, E1, E2), ETS2, error(Message)) :-
   evaluate_rhs(ETS, RS, E1, ETS1, _),
   evaluate_rhs(ETS1, RS, E2, ETS2, error(Message)).



% The addition operator.
evaluate_rhs(ETS, RS, binop(add, E1, E2), ETS2, [numbertype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   C is A + B.



% The subtraction operator.
evaluate_rhs(ETS, RS, binop(sub, E1, E2), ETS2, [numbertype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   C is A - B.



% The multiplication operator.
evaluate_rhs(ETS, RS, binop(mul, E1, E2), ETS2, [numbertype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   C is A * B.



% The division operator.
evaluate_rhs(ETS, RS, binop(div, E1, E2), ETS2, [numbertype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   C is A / B.



% The modulo operator.
evaluate_rhs(ETS, RS, binop(mod, E1, E2), ETS2, [numbertype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   C is A mod B.



% The exponent operator.
evaluate_rhs(ETS, RS, binop(pow, E1, E2), ETS2, [numbertype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   C is A ** B.



% The equality operator.
evaluate_rhs(ETS, RS, binop(eq, E1, E2), ETS2, [booleantype(false)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V1 | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V2 | _]),
   'std:type'(V1, T1),
   'std:type'(V2, T2),
   T1 \= T2.

evaluate_rhs(ETS, RS, binop(eq, E1, E2), ETS2, [booleantype(true)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V1 | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V2 | _]),
   'std:rawvalue'(V1, RV),
   'std:rawvalue'(V2, RV).

evaluate_rhs(ETS, RS, binop(eq, E1, E2), ETS2, [booleantype(false)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V1 | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V2 | _]),
   'std:rawvalue'(V1, RV1),
   'std:rawvalue'(V2, RV2),
   RV1 \= RV2.



% The less-than operator.
evaluate_rhs(ETS, RS, binop(lt, E1, E2), ETS2, [booleantype(true)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   A < B.

evaluate_rhs(ETS, RS, binop(lt, E1, E2), ETS2, [booleantype(false)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   A >= B.

evaluate_rhs(ETS, RS, binop(lt, E1, E2), ETS2, [booleantype(true)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [stringtype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [stringtype(B) | _]),
   A @< B.

evaluate_rhs(ETS, RS, binop(lt, E1, E2), ETS2, [booleantype(false)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [stringtype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [stringtype(B) | _]),
   A @>= B.

evaluate_rhs(ETS, RS, binop(lt, E1, _), ETS1, error('Left operand is not a number or string.')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V | _]),
   \+member(V, [numbertype(_), stringtype(_)]).

evaluate_rhs(ETS, RS, binop(lt, E1, E2), ETS2, error('Right operand is not a number or string.')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V1 | _]),
   member(V1, [numbertype(_), stringtype(_)]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V2 | _]),
   \+member(V2, [numbertype(_), stringtype(_)]).

evaluate_rhs(ETS, RS, binop(lt, E1, E2), ETS2, error('Right operand is not a number.')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(_) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V | _]),
   V \= numbertype(_).

evaluate_rhs(ETS, RS, binop(lt, E1, E2), ETS2, error('Right operand is not a string.')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [stringtype(_) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V | _]),
   V \= stringtype(_).



% The less-than-or-equal operator.
evaluate_rhs(ETS, RS, binop(le, E1, E2), ETS2, [booleantype(true)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   A =< B.

evaluate_rhs(ETS, RS, binop(le, E1, E2), ETS2, [booleantype(false)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   A > B.

evaluate_rhs(ETS, RS, binop(le, E1, E2), ETS2, [booleantype(true)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [stringtype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [stringtype(B) | _]),
   A @=< B.

evaluate_rhs(ETS, RS, binop(le, E1, E2), ETS2, [booleantype(false)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [stringtype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [stringtype(B) | _]),
   A @> B.

evaluate_rhs(ETS, RS, binop(le, E1, _), ETS1, error('Left operand is not a number or string.')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V | _]),
   \+member(V, [numbertype(_), stringtype(_)]).

evaluate_rhs(ETS, RS, binop(le, E1, E2), ETS2, error('Right operand is not a number or string.')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V1 | _]),
   member(V1, [numbertype(_), stringtype(_)]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V2 | _]),
   \+member(V2, [numbertype(_), stringtype(_)]).

evaluate_rhs(ETS, RS, binop(le, E1, E2), ETS2, error('Right operand is not a number.')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(_) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V | _]),
   V \= numbertype(_).

evaluate_rhs(ETS, RS, binop(le, E1, E2), ETS2, error('Right operand is not a string.')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [stringtype(_) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V | _]),
   V \= stringtype(_).



% The greater-than operator.
evaluate_rhs(ETS, RS, binop(gt, E1, E2), ETS1, VS) :-
   evaluate_rhs(ETS, RS, binop(lt, E2, E1), ETS1, VS).



% The greater-than-or-equal operator.
evaluate_rhs(ETS, RS, binop(ge, E1, E2), ETS1, VS) :-
   evaluate_rhs(ETS, RS, binop(le, E2, E1), ETS1, VS).



% The and operator.
evaluate_rhs(ETS, RS, binop(and, E1, _), ETS1, [V]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]).

evaluate_rhs(ETS, RS, binop(and, E1, E2), ETS2, VS) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_rhs(ETS1, RS, E2, ETS2, VS).



% The or operator.
evaluate_rhs(ETS, RS, binop(or, E1, _), ETS1, [V]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]).

evaluate_rhs(ETS, RS, binop(or, E1, E2), ETS2, VS) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]),
   evaluate_rhs(ETS1, RS, E2, ETS2, VS).



% The concatenation operator.
evaluate_rhs(ETS, RS, binop(concat, E1, E2), ETS2, [stringtype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [stringtype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [stringtype(B) | _]),
   atom_concat(A, B, C).

evaluate_rhs(ETS, RS, binop(concat, E1, E2), ETS2, [numbertype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [numbertype(A) | _]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [numbertype(B) | _]),
   atom_length(B, Length),
   C is ((A * (10 ** Length)) + B).

evaluate_rhs(ETS, RS, binop(concat, E1, E2), ETS2, [stringtype(C)]) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V1 | _]),
   member(V1, [numbertype(_), stringtype(_)]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V2 | _]),
   member(V2, [numbertype(_), stringtype(_)]),
   'std:type'(V1, T1),
   'std:type'(V2, T2),
   T1 \= T2,
   'std:rawvalue'(V1, A),
   'std:rawvalue'(V2, B),
   atom_concat(A, B, C).

evaluate_rhs(ETS, RS, binop(concat, E1, _), ETS1, error('Left operand is not a number or string')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V | _]),
   \+member(V, [numbertype(_), stringtype(_)]).

evaluate_rhs(ETS, RS, binop(concat, E1, E2), ETS2, error('Right operand is not a number or string')) :-
   evaluate_rhs(ETS, RS, E1, ETS1, [V1 | _]),
   member(V1, [numbertype(_), stringtype(_)]),
   evaluate_rhs(ETS1, RS, E2, ETS2, [V2 | _]),
   \+member(V2, [numbertype(_), stringtype(_)]).
                                                                                                            */


% Evaluate a function definition.
evaluate_rhs(ENV0, functiondef(PS, SS), ENV0, [function(PS, SS, [])]).



% Evaluate a function call.
evaluate_rhs(ENV0, functioncall(E, _), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).
evaluate_rhs(ENV0, functioncall(E, _), ENV1, error('Expression is not callable')) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   V \= referencetype(_, _, _).
evaluate_rhs(ENV0, functioncall(E, ES), ENV4, VS_ss) :-
   evaluate_rhs(ENV0, E, ENV1, [referencetype(function, ECID, Address) | _]),
   evaluate_rhs(ENV1, explist(ES), ENV2, VS_es),
   'env:getValue'(ENV2, ECID, Address, function(PS, SS, [])),
   'env:addContext'(ENV2, PS, VS_es, ENV3),
   evaluate_stat(ENV3, statementlist(SS), [_ | ENV4], _, VS_ss).
evaluate_rhs(ENV0, functioncall(E, ES), ENV4, VS_ss) :-
   evaluate_rhs(ENV0, E, ENV1, [referencetype(function, ECID, Address) | _]),
   evaluate_rhs(ENV1, explist(ES), ENV2, VS_es),
   'env:getValue'(ENV2, ECID, Address, function(PS, SS, ENVf)),
   ENVf \== [],
   'env:addContext'(ENVf, PS, VS_es, ENV3),
   evaluate_stat(ENV3, statementlist(SS), [_ | ENV4], _, VS_ss).


evaluate_rhs(ENV0, functioncall(E, _), ENV1, error('Metatables not implemented yet!')) :-             % TODO
   evaluate_rhs(ENV0, E, ENV1, [referencetype(table, _, _) | _]).








% Statement evaluation.
% ------------------------------------------------------------------------------

% Evaluate a list of statements.
evaluate_stat(ENV0, statementlist([]), ENV0, continue, []).
evaluate_stat(ENV0, statementlist([S | _]), ENV1, return, VS) :-
   evaluate_stat(ENV0, S, ENV1, return, VS).
evaluate_stat(ENV0, statementlist([S | _]), ENV1, break, []) :-
   evaluate_stat(ENV0, S, ENV1, break, []).
evaluate_stat(ENV0, statementlist([S | _]), ENV1, error, Error) :-
   evaluate_stat(ENV0, S, ENV1, error, Error).
evaluate_stat(ENV0, statementlist([S | SS]), ENV2, CTRL, VS) :-
   evaluate_stat(ENV0, S, ENV1, continue, _),
   evaluate_stat(ENV1, statementlist(SS), ENV2, CTRL, VS).




                                                                                                      /*


% The assignment statement.
evaluate_stat(ETS, RS, assign(LHS, _), ETS1, error, error(Message)) :-
   evaluate_lhs(ETS, RS, explist(LHS), ETS1, error(Message)).

evaluate_stat(ETS, RS, assign(LHS, RHS), ETS2, error, error(Message)) :-
   evaluate_lhs(ETS, RS, explist(LHS), ETS1, _),
   evaluate_rhs(ETS1, RS, explist(RHS), ETS2, error(Message)).

evaluate_stat(ETS, RS, assign(LHS, RHS), ETS3, continue, []) :-
   evaluate_lhs(ETS, RS, explist(LHS), ETS1, VS_lhs),
   evaluate_rhs(ETS1, RS, explist(RHS), ETS2, VS_rhs),
   env_setvalues(ETS2, VS_lhs, VS_rhs, ETS3).



% The function statement.
evaluate_stat(ETS, RS, functioncall(E, ES), ETS1, error, error(Message)) :-
   evaluate_rhs(ETS, RS, functioncall(E, ES), ETS1, error(Message)).

evaluate_stat(ETS, RS, functioncall(E, ES), ETS1, continue, []) :-
   evaluate_rhs(ETS, RS, functioncall(E, ES), ETS1, _).



% The do statement.
evaluate_stat(ETS, RS, do(SS), ETS2, C, VS) :-
   env_make(ETS, RS, ETS1, RS1),
   evaluate_stat(ETS1, RS1, statementlist(SS), ETS2, C, VS).



% The while-do statement.
evaluate_stat(ETS, RS, while(E, _), ETS1, error, error(Message)) :-
   evaluate_rhs(ETS, RS, E, ETS1, error(Message)).

evaluate_stat(ETS, RS, while(E, _), ETS1, continue, []) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]).

evaluate_stat(ETS, RS, while(E, SS), ETS3, C, VS_ss) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ETS1, RS, do(SS), ETS2, continue, _),
   evaluate_stat(ETS2, RS, while(E, SS), ETS3, C, VS_ss).

evaluate_stat(ETS, RS, while(E, SS), ETS2, C, VS_ss) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ETS1, RS, do(SS), ETS2, C, VS_ss),
   member(C, [return, error]).

evaluate_stat(ETS, RS, while(E, SS), ETS2, continue, []) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ETS1, RS, do(SS), ETS2, break, _).



% The repeat-until statement.
evaluate_stat(ETS, RS, repeat(_, SS), ETS2, C, VS) :-
   env_make(ETS, RS, ETS1, RS1),
   evaluate_stat(ETS1, RS1, statementlist(SS), ETS2, C, VS),
   member(C, [return, error]).

evaluate_stat(ETS, RS, repeat(_, SS), ETS2, continue, []) :-
   env_make(ETS, RS, ETS1, RS1),
   evaluate_stat(ETS1, RS1, statementlist(SS), ETS2, break, []).

evaluate_stat(ETS, RS, repeat(E, SS), ETS3, error, error(Message)) :-
   env_make(ETS, RS, ETS1, RS1),
   evaluate_stat(ETS1, RS1, statementlist(SS), ETS2, continue, _),
   evaluate_rhs(ETS2, RS1, E, ETS3, error(Message)).

evaluate_stat(ETS, RS, repeat(E, SS), ETS3, continue, []) :-
   env_make(ETS, RS, ETS1, RS1),
   evaluate_stat(ETS1, RS1, statementlist(SS), ETS2, continue, _),
   evaluate_rhs(ETS2, RS1, E, ETS3, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]).

evaluate_stat(ETS, RS, repeat(E, SS), ETS4, C, VS) :-
   env_make(ETS, RS, ETS1, RS1),
   evaluate_stat(ETS1, RS1, statementlist(SS), ETS2, continue, _),
   evaluate_rhs(ETS2, RS1, E, ETS3, [V | _]),
   member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ETS3, RS, repeat(E, SS), ETS4, C, VS).



% If statement.
evaluate_stat(ETS, RS, if(E, _, _), ETS1, error, error(Message)) :-
   evaluate_rhs(ETS, RS, E, ETS1, error(Message)).

evaluate_stat(ETS, RS, if(E, _, S), ETS2, C, VS) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ETS1, RS, S, ETS2, C, VS).

evaluate_stat(ETS, RS, if(E, S, _), ETS2, C, VS) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ETS1, RS, S, ETS2, C, VS).
                                                                                                      */



evaluate_stat([EC | ECS], localvariable(N, E), [EC2 | ECS1], continue, []) :-
   evaluate_rhs([EC | ECS], E, [EC1 | ECS1], [V | _]),
   'env:setValue'(EC1, N, V, EC2).
evaluate_stat(ENV0, localvariable(_, E), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



% The return statement.
evaluate_stat(ENV0, return(ES), ENV1, return, VS) :-
   evaluate_rhs(ENV0, explist(ES), ENV1, VS),
   VS \= error(_).
evaluate_stat(ENV0, return(ES), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, explist(ES), ENV1, error(Message)).



% Break statement.
evaluate_stat(ENV0, break, ENV0, break, []).
