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
evaluate_lhs(ENV0, explist([E | ES]), ENV2, [[ECID, K] | AS]) :-
   evaluate_lhs(ENV0, E, ENV1, [ECID, K]), !,
   evaluate_lhs(ENV1, explist(ES), ENV2, AS), !.
evaluate_lhs(ENV0, explist([E | _]), ENV1, error(Message)) :-
   evaluate_lhs(ENV0, E, ENV1, error(Message)), !.
evaluate_lhs(ENV0, explist([E | ES]), ENV2, error(Message)) :-
   evaluate_lhs(ENV0, E, ENV1, _), !,
   evaluate_lhs(ENV1, explist(ES), ENV2, error(Message)).



% Evaluate a left-hand side variable.
evaluate_lhs([EC], variable(N), [EC], [ECID, N]) :-
   'env:getIdentifier'(EC, ECID), !.
evaluate_lhs([EC | ECS], variable(N), [EC | ECS], [ECID, N]) :-
   'env:keyExists'(EC, N), !,
   'env:getIdentifier'(EC, ECID).
evaluate_lhs([EC | ECS], variable(N), [EC | ECS], [ECID, N]) :-
   \+'env:keyExists'(EC, N), !,
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
evaluate_rhs(ENV0, explist([E]), ENV1, VS) :-
   E \== '...',
   evaluate_rhs(ENV0, E, ENV1, VS), !.
evaluate_rhs(ENV0, explist(['...']), ENV1, []) :-
   evaluate_rhs(ENV0, '...', ENV1, [niltype(nil)]), !.
evaluate_rhs(ENV0, explist(['...']), ENV1, [EC | ECS]) :-
   evaluate_rhs(ENV0, '...', ENV1, [EC | ECS]), !.
evaluate_rhs(ENV0, explist([E | ES]), ENV2, [VE | VS]) :-
   ES \= [],
   evaluate_rhs(ENV0, E, ENV1, [VE | _]), !,
   evaluate_rhs(ENV1, explist(ES), ENV2, VS), !.
evaluate_rhs(ENV0, explist([E | _]), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)), !.
evaluate_rhs(ENV0, explist([E | ES]), ENV2, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, _), !,
   evaluate_rhs(ENV1, explist(ES), ENV2, error(Message)).



% Evaluate values.
evaluate_rhs(ENV0, niltype(nil), ENV0, [niltype(nil)]).
evaluate_rhs(ENV0, booleantype(B), ENV0, [booleantype(B)]) :-
   member(B, [false, true]).
evaluate_rhs(ENV0, numbertype(N), ENV0, [numbertype(N)]).
evaluate_rhs(ENV0, stringtype(S), ENV0, [stringtype(S)]).
evaluate_rhs(ENV0, referencetype(Type, ECID, K), ENV0, [referencetype(Type, ECID, K)]) :-
   member(Type, [table, function]).








































% Table constructor.
evaluate_rhs(ENV0, tableconstructor([]), [EC1 | ECS], [Reference]) :-
   [EC | ECS] = ENV0,
   'env:addObject'(EC, table([]), EC1, Reference), !.
evaluate_rhs(ENV0, tableconstructor([_ | _]), [EC1 | ECS], [Reference]) :-
   [EC | ECS] = ENV0,
   'env:addObject'(EC, table(['???']), EC1, Reference).































% Enclosed expressions.
evaluate_rhs(ENV0, enclosed(E), ENV1, []) :-
   evaluate_rhs(ENV0, E, ENV1, []), !.
evaluate_rhs(ENV0, enclosed(E), ENV1, [V]) :-
   evaluate_rhs(ENV0, E, ENV1, VS),
   VS \= error(_),
   [V | _] = VS, !.
evaluate_rhs(ENV0, enclosed(E), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



% Evaluate a right-hand side variable.
evaluate_rhs(ENV0, variable(N), ENV1, [V]) :-
   evaluate_lhs(ENV0, variable(N), ENV1, [ECID, N]),
   'env:getValue'(ENV1, ECID, N, V), !.
evaluate_rhs(ENV0, variable(N), ENV1, error(Message)) :-
   evaluate_lhs(ENV0, variable(N), ENV1, error(Message)).



                                                                                                            /*
% Evaluate a right-hand side field accessor.
evaluate_rhs(ETS, RS, access(R, K), ETS1, error(Message)) :-
   evaluate_lhs(ETS, RS, access(R, K), ETS1, error(Message)).

evaluate_rhs(ETS, RS, access(R, K), ETS1, [V]) :-
   evaluate_lhs(ETS, RS, access(R, K), ETS1, [R1, K1]),
   env_getvalue(ETS1, R1, K1, V).
                                                                                                            */



% Evaluate a variadic expression.
evaluate_rhs([EC], '...', [EC], error('\'...\' is not defined.')) :-
   \+'env:keyExists'(EC, '...'), !.
evaluate_rhs([EC], '...', [EC], [V | VS]) :-
   'env:keyExists'(EC, '...'),
   'env:getValue'(EC, '...', [V | VS]), !.
evaluate_rhs([EC | ECS], '...', [EC | ECS], VS) :-
   'env:keyExists'(EC, '...'),
   'env:getValue'(EC, '...', VS),
   VS \== [], !.
evaluate_rhs([EC | ECS], '...', [EC | ECS], [niltype(nil)]) :-
   'env:keyExists'(EC, '...'),
   'env:getValue'(EC, '...', []), !.
evaluate_rhs([EC | ECS], '...', [EC | ECS], VS) :-
   \+'env:keyExists'(EC, '...'),
   evaluate_rhs(ECS, '...', ECS, VS).



% The type operator.
evaluate_rhs(ENV0, unop(type, E), ENV1, [type(Type)]) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   'std:type'(V, Type), !.



% The unary minus operator.
evaluate_rhs(ENV0, unop(unm, E), ENV1, [numbertype(M)]) :-
   evaluate_rhs(ENV0, E, ENV1, [numbertype(N) | _]),
   M is -N, !.



% The not operator.
evaluate_rhs(ENV0, unop(not, E), ENV1, [booleantype(true)]) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]), !.
evaluate_rhs(ENV0, unop(not, E), ENV1, [booleantype(false)]) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]), !.



% The length operator.
evaluate_rhs(ENV0, unop(len, E), ENV1, [numbertype(Size)]) :-
   evaluate_rhs(ENV0, E, ENV1, [table(M) | _]),
   'map:size'(M, Size), !.
evaluate_rhs(ENV0, unop(len, E), ENV1, [numbertype(Size)]) :-
   evaluate_rhs(ENV0, E, ENV1, [referencetype(table, ECID, K) | _]),
   'env:getValue'(ENV1, ECID, K, table(M)),
   'map:size'(M, Size), !.
evaluate_rhs(ENV0, unop(len, E), ENV1, [numbertype(Size)]) :-
   evaluate_rhs(ENV0, E, ENV1, [stringtype(S) | _]),
   atom_length(S, Size), !.
evaluate_rhs(ENV0, unop(len, E), ENV1, error('\'#\' operand is not a table or string.')) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   \+member(V, [table(_), referencetype(table, _, _), stringtype(_)]), !.



% Generic error handling for unary operators.
evaluate_rhs(ENV0, unop(_, E), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



% The addition operator.
evaluate_rhs(ENV0, binop(add, E1, E2), ENV2, [numbertype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   C is A + B, !.



% The subtraction operator.
evaluate_rhs(ENV0, binop(sub, E1, E2), ENV2, [numbertype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   C is A - B, !.



% The multiplication operator.
evaluate_rhs(ENV0, binop(mul, E1, E2), ENV2, [numbertype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   C is A * B, !.



% The division operator.
evaluate_rhs(ENV0, binop(div, E1, E2), ENV2, [numbertype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   C is A / B, !.



% The modulo operator.
evaluate_rhs(ENV0, binop(mod, E1, E2), ENV2, [numbertype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   C is A mod B, !.



% The exponent operator.
evaluate_rhs(ENV0, binop(pow, E1, E2), ENV2, [numbertype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   C is A ** B, !.



% The equality operator.
evaluate_rhs(ENV0, binop(eq, E1, E2), ENV2, [booleantype(false)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   'std:type'(V1, T1),
   'std:type'(V2, T2),
   T1 \= T2, !.
evaluate_rhs(ENV0, binop(eq, E1, E2), ENV2, [booleantype(true)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   'std:rawValue'(V1, RV),
   'std:rawValue'(V2, RV), !.
evaluate_rhs(ENV0, binop(eq, E1, E2), ENV2, [booleantype(false)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   'std:rawValue'(V1, RV1),
   'std:rawValue'(V2, RV2),
   RV1 \= RV2, !.



% The less-than operator.
evaluate_rhs(ENV0, binop(lt, E1, E2), ENV2, [booleantype(true)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   A < B, !.
evaluate_rhs(ENV0, binop(lt, E1, E2), ENV2, [booleantype(false)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   A >= B, !.
evaluate_rhs(ENV0, binop(lt, E1, E2), ENV2, [booleantype(true)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [stringtype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [stringtype(B) | _]),
   A @< B, !.
evaluate_rhs(ENV0, binop(lt, E1, E2), ENV2, [booleantype(false)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [stringtype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [stringtype(B) | _]),
   A @>= B, !.
evaluate_rhs(ENV0, binop(lt, E1, _), ENV1, error('Left operand is not a number or string.')) :-
   evaluate_rhs(ENV0, E1, ENV1, [V | _]),
   \+member(V, [numbertype(_), stringtype(_)]), !.
evaluate_rhs(ENV0, binop(lt, E1, E2), ENV2, error('Right operand is not a number or string.')) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   member(V1, [numbertype(_), stringtype(_)]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   \+member(V2, [numbertype(_), stringtype(_)]), !.



% The less-than-or-equal operator.
evaluate_rhs(ENV0, binop(le, E1, E2), ENV2, [booleantype(true)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   A =< B, !.
evaluate_rhs(ENV0, binop(le, E1, E2), ENV2, [booleantype(false)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   A > B, !.
evaluate_rhs(ENV0, binop(le, E1, E2), ENV2, [booleantype(true)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [stringtype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [stringtype(B) | _]),
   A @=< B, !.
evaluate_rhs(ENV0, binop(le, E1, E2), ENV2, [booleantype(false)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [stringtype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [stringtype(B) | _]),
   A @> B, !.
evaluate_rhs(ENV0, binop(le, E1, _), ENV1, error('Left operand is not a number or string.')) :-
   evaluate_rhs(ENV0, E1, ENV1, [V | _]),
   \+member(V, [numbertype(_), stringtype(_)]), !.
evaluate_rhs(ENV0, binop(le, E1, E2), ENV2, error('Right operand is not a number or string.')) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   member(V1, [numbertype(_), stringtype(_)]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   \+member(V2, [numbertype(_), stringtype(_)]), !.



% The and operator.
evaluate_rhs(ENV0, binop(and, E1, _), ENV1, [V]) :-
   evaluate_rhs(ENV0, E1, ENV1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]), !.
evaluate_rhs(ENV0, binop(and, E1, E2), ENV2, VS) :-
   evaluate_rhs(ENV0, E1, ENV1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_rhs(ENV1, E2, ENV2, VS), !.



% The or operator.
evaluate_rhs(ENV0, binop(or, E1, _), ENV1, [V]) :-
   evaluate_rhs(ENV0, E1, ENV1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]), !.
evaluate_rhs(ENV0, binop(or, E1, E2), ENV2, VS) :-
   evaluate_rhs(ENV0, E1, ENV1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]),
   evaluate_rhs(ENV1, E2, ENV2, VS), !.



% The concatenation operator.
evaluate_rhs(ENV0, binop(concat, E1, E2), ENV2, [stringtype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [stringtype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [stringtype(B) | _]),
   atom_concat(A, B, C), !.
evaluate_rhs(ENV0, binop(concat, E1, E2), ENV2, [numbertype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [numbertype(A) | _]),
   evaluate_rhs(ENV1, E2, ENV2, [numbertype(B) | _]),
   atom_length(B, Length),
   C is ((A * (10 ** Length)) + B), !.
evaluate_rhs(ENV0, binop(concat, E1, E2), ENV2, [stringtype(C)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   member(V1, [numbertype(_), stringtype(_)]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   member(V2, [numbertype(_), stringtype(_)]),
   'std:type'(V1, T1),
   'std:type'(V2, T2),
   T1 \= T2,
   'std:rawValue'(V1, A),
   'std:rawValue'(V2, B),
   atom_concat(A, B, C), !.
evaluate_rhs(ENV0, binop(concat, E1, _), ENV1, error('Left operand is not a number or string')) :-
   evaluate_rhs(ENV0, E1, ENV1, [V | _]),
   \+member(V, [numbertype(_), stringtype(_)]), !.
evaluate_rhs(ENV0, binop(concat, E1, E2), ENV2, error('Right operand is not a number or string')) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   member(V1, [numbertype(_), stringtype(_)]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   \+member(V2, [numbertype(_), stringtype(_)]), !.



% Generic error handling for binary operators.
evaluate_rhs(ENV0, binop(_, E1, _), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E1, ENV1, error(Message)), !.
evaluate_rhs(ENV0, binop(_, E1, E2), ENV2, error(Message)) :-
   evaluate_rhs(ENV0, E1, ENV1, _),
   evaluate_rhs(ENV1, E2, ENV2, error(Message)), !.



% Evaluate a function definition.
evaluate_rhs([EC | ECS], functiondef(PS, SS), [EC1 | ECS], [Reference]) :-
   'env:addObject'(EC, function(PS, SS, []), EC1, Reference).



% Evaluate a function call.
evaluate_rhs(ENV0, functioncall(E, ES), ENV4, VS_ss) :-
   evaluate_rhs(ENV0, E, ENV1, [referencetype(function, ECID, Address) | _]),
   evaluate_rhs(ENV1, explist(ES), ENV2, VS_es),
   'env:getValue'(ENV2, ECID, Address, function(PS, SS, [])),
   'env:addContext'(ENV2, PS, VS_es, ENV3), !,
   evaluate_stat(ENV3, statementlist(SS), [_ | ENV4], _, VS_ss), !.
evaluate_rhs(ENV0, functioncall(E, ES), ENV4, VS_ss) :-
   evaluate_rhs(ENV0, E, ENV1, [referencetype(function, ECID, Address) | _]),
   evaluate_rhs(ENV1, explist(ES), ENV2, VS_es),
   'env:getValue'(ENV2, ECID, Address, function(PS, SS, ENVf)),
   ENVf \= [],
   'env:addContext'(ENVf, PS, VS_es, ENV3), !,
   evaluate_stat(ENV3, statementlist(SS), [_ | ENV4], _, VS_ss), !.

evaluate_rhs(ENV0, functioncall(E, _), ENV1, error('Metatables not implemented yet!')) :-             % TODO
   evaluate_rhs(ENV0, E, ENV1, [referencetype(table, _, _) | _]), !.


evaluate_rhs(ENV0, functioncall(E, _), ENV1, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)), !.
evaluate_rhs(ENV0, functioncall(E, _), ENV1, error('Expression is not a callable object.')) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   V \= referencetype(_, _, _), !.






% Statement evaluation.
% ------------------------------------------------------------------------------

% Evaluate a list of statements.
evaluate_stat(ENV0, statementlist([]), ENV0, continue, []).
evaluate_stat(ENV0, statementlist([S | SS]), ENV2, CTRL, VS) :-
   evaluate_stat(ENV0, S, ENV1, continue, _), !,
   evaluate_stat(ENV1, statementlist(SS), ENV2, CTRL, VS), !.
evaluate_stat(ENV0, statementlist([S | _]), ENV1, CTRL, VS) :-
   evaluate_stat(ENV0, S, ENV1, CTRL, VS),
   member(CTRL, [return, break, error]).



% The assignment statement.
evaluate_stat(ENV0, assign(LHS, RHS), ENV3, continue, []) :-
   evaluate_lhs(ENV0, explist(LHS), ENV1, VS_lhs),
   VS_lhs \= error(_),
   evaluate_rhs(ENV1, explist(RHS), ENV2, VS_rhs),
   VS_rhs \= error(_), !,
   'env:setValues'(ENV2, VS_lhs, VS_rhs, ENV3).
evaluate_stat(ENV0, assign(LHS, _), ENV1, error, error(Message)) :-
   evaluate_lhs(ENV0, explist(LHS), ENV1, error(Message)), !.
evaluate_stat(ENV0, assign(LHS, RHS), ENV2, error, error(Message)) :-
   evaluate_lhs(ENV0, explist(LHS), ENV1, _),
   evaluate_rhs(ENV1, explist(RHS), ENV2, error(Message)).



% The function statement.
evaluate_stat(ENV0, functioncall(E, ES), ENV1, continue, []) :-
   evaluate_rhs(ENV0, functioncall(E, ES), ENV1, VS),
   VS \= error(_).
evaluate_stat(ENV0, functioncall(E, ES), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, functioncall(E, ES), ENV1, error(Message)), !.



% The do statement.
evaluate_stat(ENV0, do(SS), ENV2, CTRL, VS) :-
   'env:addContext'(ENV0, ENV1),
   evaluate_stat(ENV1, statementlist(SS), [_ | ENV2], CTRL, VS).



% The while-do statement.
evaluate_stat(ENV0, while(E, _), ENV1, continue, []) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]), !.
evaluate_stat(ENV0, while(E, SS), ENV2, continue, []) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ENV1, do(SS), ENV2, break, _), !.
evaluate_stat(ENV0, while(E, SS), ENV3, CTRL, VS_ss) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]), !,
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ENV1, do(SS), ENV2, continue, _), !,
   evaluate_stat(ENV2, while(E, SS), ENV3, CTRL, VS_ss), !.
evaluate_stat(ENV0, while(E, SS), ENV2, CTRL, VS_ss) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ENV1, do(SS), ENV2, CTRL, VS_ss),
   member(CTRL, [return, error]), !.
evaluate_stat(ENV0, while(E, _), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



% The repeat-until statement.
evaluate_stat(ENV0, repeat(_, SS), ENV2, CTRL, VS) :-
   'env:addContext'(ENV0, ENV1),
   evaluate_stat(ENV1, statementlist(SS), [_ | ENV2], CTRL, VS),
   member(CTRL, [return, error]), !.
evaluate_stat(ENV0, repeat(_, SS), ENV2, continue, []) :-
   'env:addContext'(ENV0, ENV1),
   evaluate_stat(ENV1, statementlist(SS), [_ | ENV2], break, []), !.
evaluate_stat(ENV0, repeat(E, SS), ENV3, continue, []) :-
   'env:addContext'(ENV0, ENV1),
   evaluate_stat(ENV1, statementlist(SS), ENV2, continue, _),
   evaluate_rhs(ENV2, E, [_ | ENV3], [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]), !.
evaluate_stat(ENV0, repeat(E, SS), ENV4, CTRL, VS) :-
   'env:addContext'(ENV0, ENV1),
   evaluate_stat(ENV1, statementlist(SS), ENV2, continue, _),
   evaluate_rhs(ENV2, E, [_ | ENV3], [V | _]), !,
   member(V, [niltype(nil), booleantype(false)]),
   evaluate_stat(ENV3, repeat(E, SS), ENV4, CTRL, VS), !.
evaluate_stat(ENV0, repeat(E, SS), ENV3, error, error(Message)) :-
   'env:addContext'(ENV0, ENV1),
   evaluate_stat(ENV1, statementlist(SS), ENV2, continue, _),
   evaluate_rhs(ENV2, E, [_ | ENV3], error(Message)).



% If statement.
evaluate_stat(ENV0, if(E, _, S), ENV2, CTRL, VS) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   member(V, [niltype(nil), booleantype(false)]), !,
   evaluate_stat(ENV1, S, ENV2, CTRL, VS).
evaluate_stat(ENV0, if(E, S, _), ENV2, CTRL, VS) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   \+member(V, [niltype(nil), booleantype(false)]), !,
   evaluate_stat(ENV1, S, ENV2, CTRL, VS).
evaluate_stat(ENV0, if(E, _, _), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



evaluate_stat([EC | ECS], localvariable(N, E), [EC2 | ECS1], continue, []) :-
   evaluate_rhs([EC | ECS], E, [EC1 | ECS1], [V | _]),
   'env:setValue'(EC1, N, V, EC2), !.
evaluate_stat(ENV0, localvariable(_, E), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



% The return statement.
evaluate_stat(ENV0, return(ES), ENV1, return, VS) :-
   evaluate_rhs(ENV0, explist(ES), ENV1, VS),
   VS \= error(_), !.
evaluate_stat(ENV0, return(ES), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, explist(ES), ENV1, error(Message)).



% Break statement.
evaluate_stat(ENV0, break, ENV0, break, []).



% Evaluate a chunk.
'evaluate:chunk'([], _, [], []).
'evaluate:chunk'(Statements, Arguments, Environment, Result) :-
   'env:addContext'([], ['...'], Arguments, [context(ECID, M)]),
   'env:0'(ECID, M0),
   append(M, M0, M1),
   evaluate_stat([context(0, M1)], statementlist(Statements), Environment, _, Result).
