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
evaluate_lhs(ETS, _, explist([]), ETS, []).

evaluate_lhs(ETS, RS, explist([E | _]), ETS1, error(Message)) :-
   evaluate_lhs(ETS, RS, E, ETS1, error(Message)), !.

evaluate_lhs(ETS, RS, explist([E | ES]), ETS2, error(Message)) :-
   evaluate_lhs(ETS, RS, E, ETS1, _),
   evaluate_lhs(ETS1, RS, explist(ES), ETS2, error(Message)), !.

evaluate_lhs(ETS, RS, explist([E | ES]), ETS2, Result) :-
   evaluate_lhs(ETS, RS, E, ETS1, [R, K]),
   evaluate_lhs(ETS1, RS, explist(ES), ETS2, VS),
   append([[R, K]], VS, Result).



% Evaluate a left-hand side variable.
evaluate_lhs(ETS, [R | _], variable(N), ETS, [R, N]) :-
   env_keyexists(ETS, R, N, booleantype(true)).

evaluate_lhs(ETS, [R], variable(N), ETS, [R, N]).

evaluate_lhs(ETS, [R_i | RS], variable(N), ETS, Address) :-
   env_keyexists(ETS, R_i, N, booleantype(false)),
   evaluate_lhs(ETS, RS, variable(N), _, Address).



% Evaluate a left-hand side field accessor.
evaluate_lhs(ETS, RS, access(R, _), ETS1, error(Message)) :-
   evaluate_rhs(ETS, RS, R, ETS1, error(Message)).

evaluate_lhs(ETS, RS, access(R, K), ETS2, error(Message)) :-
   evaluate_rhs(ETS, RS, R, ETS1, _),
   evaluate_rhs(ETS1, RS, K, ETS2, error(Message)).

evaluate_lhs(ETS, RS, access(R, K), ETS2, [V_r, V_k]) :-
   evaluate_rhs(ETS, RS, R, ETS1, [V_r | _]),
   evaluate_rhs(ETS1, RS, K, ETS2, [V_k | _]).



% Evaluate a list of right-hand side (RHS) expressions.
evaluate_rhs(ETS, _, explist([]), ETS, []).

evaluate_rhs(ETS, RS, explist([E | _]), ETS1, error(Message)) :-
   evaluate_rhs(ETS, RS, E, ETS1, error(Message)), !.

evaluate_rhs(ETS, RS, explist([E | ES]), ETS2, error(Message)) :-
   evaluate_rhs(ETS, RS, E, ETS1, _),
   evaluate_rhs(ETS1, RS, explist(ES), ETS2, error(Message)), !.

evaluate_rhs(ETS, RS, explist([E]), ETS1, VS) :-
   evaluate_rhs(ETS, RS, E, ETS1, VS).

evaluate_rhs(ETS, RS, explist([E | ES]), ETS2, [VE | VS]) :-
   evaluate_rhs(ETS, RS, E, ETS1, [VE | _]),
   evaluate_rhs(ETS1, RS, explist(ES), ETS2, VS).



% Values.
evaluate_rhs(ETS, _, niltype(nil), ETS, [niltype(nil)]).
evaluate_rhs(ETS, _, booleantype(B), ETS, [booleantype(B)]).
evaluate_rhs(ETS, _, numbertype(N), ETS, [numbertype(N)]).
evaluate_rhs(ETS, _, stringtype(S), ETS, [stringtype(S)]).
evaluate_rhs(ETS, _, referencetype(N), ETS, [referencetype(N)]).
evaluate_rhs(ETS, _, functiontype(PS, SS, RS), ETS, [functiontype(PS, SS, RS)]).



% Table constructor.
evaluate_rhs(ETS, RS, tableconstructor(FS), ETS1, [R]) :-
   env_make(ETS, RS, FS, ETS1, [R | _]).



% Enclosed expressions.
evaluate_rhs(ETS, RS, enclosed(E), ETS1, error(Message)) :-
   evaluate_rhs(ETS, RS, E, ETS1, error(Message)), !.

evaluate_rhs(ETS, RS, enclosed(E), ETS1, []) :-
   evaluate_rhs(ETS, RS, E, ETS1, []).

evaluate_rhs(ETS, RS, enclosed(E), ETS1, [V]) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]).



% Evaluate a right-hand side variable.
evaluate_rhs(ETS, RS, variable(N), ETS1, error(Message)) :-
   evaluate_lhs(ETS, RS, variable(N), ETS1, error(Message)), !.

evaluate_rhs(ETS, RS, variable(N), ETS, [V]) :-
   evaluate_lhs(ETS, RS, variable(N), _, [R, K]),
   env_getvalue(ETS, R, K, V).



% Evaluate a right-hand side field accessor.
evaluate_rhs(ETS, RS, access(R, K), ETS1, error(Message)) :-
   evaluate_lhs(ETS, RS, access(R, K), ETS1, error(Message)), !.

evaluate_rhs(ETS, RS, access(R, K), ETS1, [V]) :-
   evaluate_lhs(ETS, RS, access(R, K), ETS1, [R1, K1]),
   env_getvalue(ETS1, R1, K1, V).



% Evaluate a variadic expression.
evaluate_rhs(ETS, [], '...', ETS, error('\'...\' is not defined.')).

evaluate_rhs(ETS, [R | _], '...', ETS, VS) :-
   env_keyexists(ETS, R, '...', booleantype(true)),
   env_getvalue(ETS, R, '...', VS).

evaluate_rhs(ETS, [R | RS], '...', ETS1, VS) :-
   env_keyexists(ETS, R, '...', booleantype(false)),
   evaluate_rhs(ETS, RS, '...', ETS1, VS).



% Unary operators.
evaluate_rhs(ETS, RS, unop(_, E), ETS1, error(Message)) :-
   evaluate_rhs(ETS, RS, E, ETS1, error(Message)).



% The type operator.
evaluate_rhs(ETS, RS, unop(type, E), ETS1, [type(Type)]) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   'std:type'(V, Type).



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



% Evaluate a function definition.
evaluate_rhs(ETS, _, function(PS, SS), ETS, [functiontype(PS, SS, [])]).



% Evaluate a function call.
evaluate_rhs(ETS, RS, functioncall(E, _), ETS1, error(Message)) :-
   evaluate_rhs(ETS, RS, E, ETS1, error(Message)), !.

evaluate_rhs(ETS, RS, functioncall(E, _), ETS1, error('Expression is not a function')) :-
   evaluate_rhs(ETS, RS, E, ETS1, [V | _]),
   V \= functiontype(_, _, _).

evaluate_rhs(ETS, RS, functioncall(E, ES), ETS2, error(Message)) :-
   evaluate_rhs(ETS, RS, E, ETS1, [functiontype(_, _, _) | _]),
   evaluate_rhs(ETS1, RS, explist(ES), ETS2, error(Message)), !.

evaluate_rhs(ETS, RS, functioncall(E, ES), ETS4, VS_ss) :-
   evaluate_rhs(ETS, RS, E, ETS1, [functiontype(PS, SS, []) | _]),
   evaluate_rhs(ETS1, RS, explist(ES), ETS2, VS_es),
   env_make(ETS2, RS, PS, VS_es, ETS3, RS1),
   evaluate_stat(ETS3, RS1, statementlist(SS), ETS4, _, VS_ss).

evaluate_rhs(ETS, RS, functioncall(E, ES), ETS4, VS_ss) :-
   evaluate_rhs(ETS, RS, E, ETS1, [functiontype(PS, SS, RS_f) | _]),
   evaluate_rhs(ETS1, RS, explist(ES), ETS2, VS_es),
   env_make(ETS2, RS_f, PS, VS_es, ETS3, RS1),
   evaluate_stat(ETS3, RS1, statementlist(SS), ETS4, _, VS_ss).



% Statement evaluation.
% ------------------------------------------------------------------------------

% Evaluate a list of statements.
evaluate_stat(ETS, _, statementlist([]), ETS, continue, []).

evaluate_stat(ETS, RS, statementlist([S | _]), ETS1, return, VS) :-
   evaluate_stat(ETS, RS, S, ETS1, return, VS).

evaluate_stat(ETS, RS, statementlist([S | _]), ETS1, break, []) :-
   evaluate_stat(ETS, RS, S, ETS1, break, _).

evaluate_stat(ETS, RS, statementlist([S | _]), ETS1, error, Error) :-
   evaluate_stat(ETS, RS, S, ETS1, error, Error).

evaluate_stat(ETS, RS, statementlist([S | SS]), ETS2, C, VS) :-
   evaluate_stat(ETS, RS, S, ETS1, continue, _),
   evaluate_stat(ETS1, RS, statementlist(SS), ETS2, C, VS).



% Build the environment.
evaluate_stat([table(FS)], _, 'std:initialise', [table(FS2)], continue, []) :-
   'std:et0'(table(FS1)),
   append(FS, FS1, FS2).



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



% Local variables.
evaluate_stat(ETS, RS, localvariable(_, Expression), ETS1, error, error(Message)) :-
   evaluate_rhs(ETS, RS, Expression, ETS1, error(Message)).

evaluate_stat(ETS, [R | RS], localvariable(Name, Expression), ETS2, continue, []) :-
   evaluate_rhs(ETS, [R | RS], Expression, ETS1, [V | _]),
   env_setvalue(ETS1, R, Name, V, ETS2).



% The return statement.
evaluate_stat(ETS, RS, return(Expressions), ETS1, error, error(Message)) :-
   evaluate_rhs(ETS, RS, explist(Expressions), ETS1, error(Message)).

evaluate_stat(ETS, RS, return(Expressions), ETS1, return, VS) :-
   evaluate_rhs(ETS, RS, explist(Expressions), ETS1, VS).



% Break statement.
evaluate_stat(ETS, _, break, ETS, break, []).
