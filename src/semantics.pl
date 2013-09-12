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


% Value management.
% ------------------------------------------------------------------------------

% Return the type of a value.
value_type(niltype(_), 'nil').
value_type(booleantype(_), 'boolean').
value_type(numbertype(_), 'number').
value_type(stringtype(_), 'string').
value_type(referencetype(table, _), 'table').
value_type(referencetype(function, _), 'function').


% Return the raw value.
value_raw(niltype(nil), nil).
value_raw(booleantype(B), B).
value_raw(numbertype(N), N).
value_raw(stringtype(S), S).
value_raw(referencetype(Type, Address), [Type, Address]).


% Expression evaluation.
% ------------------------------------------------------------------------------

% Evaluate a list of left-hand side (LHS) expressions.
evaluate_lhs(ENV0, expressions([]), ENV0, []).
evaluate_lhs(ENV0, expressions([E | ES]), ENVn, Result) :-
   evaluate_lhs(ENV0, E, ENV1, VS_E),
   evaluate_lhs(ENV1, expressions(ES), ENV2, VS_ES),
   (
      VS_E \= error(_), VS_ES \= error(_) ->
      (
         ENVn = ENV2,
         Result = [VS_E | VS_ES]
      );
      (
         VS_E = error(_) ->
         (
            ENVn = ENV1,
            Result = VS_E
         );
         (
            ENVn = ENV2,
            Result = VS_ES
         )
      )
   ).



% Evaluate a left-hand side variable.
evaluate_lhs([ContextPath, Pool], variable(Name), [ContextPath, Pool], [Locator, Name]) :-
   getContext(ContextPath, Pool, Context),
   (
      keyExists(Context, Name) ->
      Locator = ContextPath;
      (
         popContext([ContextPath, _], [path(NewIndices), _]),
         (
            NewIndices = [] ->
            Locator = ContextPath;
            evaluate_lhs([path(NewIndices), Pool], variable(Name), _, [Locator, _])
         )
      )
   ).



% Evaluate a left-hand side table field accessor.
evaluate_lhs(ENV0, access(E1, E2), ENVn, Result) :-
   evaluate_rhs(ENV0, E1, ENV1, VS_E1),
   evaluate_rhs(ENV1, E2, ENV2, VS_E2),
   (
      VS_E1 \= error(_), VS_E2 \= error(_) ->
      (
         VS_E1 \= [referencetype(table, _) | _] ->
         (
            ENVn = ENV1,
            Result = error('Accessing an object that is not a table.')
         );
         (
            VS_E1 = [Locator | _],
            VS_E2 = [Key | _],
            ENVn = ENV2,
            Result = [Locator, Key]
         )
      );
      (
         VS_E1 = error(_) ->
         (
            ENVn = ENV1,
            Result = VS_E1
         );
         (
            ENVn = ENV2,
            Result = VS_E2
         )
      )
   ).



% Evaluate a list of right-hand side (RHS) expressions.
evaluate_rhs(ENV0, expressions(Expressions), ENVn, Result) :-
(
   Expressions = [] ->
   (
      Result = [],
      ENVn = ENV0
   );
   (
      [E | ES] = Expressions,
      evaluate_rhs(ENV0, E, ENV1, VS_E),
      evaluate_rhs(ENV1, expressions(ES), ENV2, VS_ES),
      (
         VS_E \= error(_), VS_ES \= error(_) ->
         (
            ENVn = ENV2,
            (
               ES = [] ->
               Result = VS_E;
               (
                  VS_E = [] ->
                  Result = [niltype(nil) | VS_ES];
                  (
                     VS_E = [V | _],
                     Result = [V | VS_ES]
                  )
               )
            )
         );
         (
            VS_E = error(_) ->
            (
               ENVn = ENV1,
               Result = VS_E
            );
            (
               ENVn = ENV2,
               Result = VS_ES
            )
         )
      )
   )
).



% Evaluate a list of fields.
evaluate_rhs(ENV0, fields(Fields), ENVn, Result) :-
(
   Fields = [] ->
   (
      ENVn = ENV0,
      Result = []
   );
   (
      Fields = [[FK, FV] | FS],
      evaluate_rhs(ENV0, FK, ENV1, VS_FK),
      evaluate_rhs(ENV1, FV, ENV2, VS_FV),
      evaluate_rhs(ENV2, fields(FS), ENV3, VS_FS),
      (
         VS_FK \= error(_), VS_FV \= error(_), VS_FS \= error(_) ->
         (
            VS_FK = [VK | _],
            VS_FV = [VV | _],
            ENVn = ENV3,
            Result = [[VK, VV] | VS_FS]
         );
         (
            VS_FK = error(_) ->
            (
               ENVn = ENV1,
               Result = VS_FK
            );
            (
               VS_FV = error(_) ->
               (
                  ENVn = ENV2,
                  Result = VS_FV
               );
               (
                  ENVn = ENV3,
                  Result = VS_FS
               )
            )
         )
      )
   )
).



% Evaluate values.
evaluate_rhs(ENV0, niltype(nil), ENV0, [niltype(nil)]).
evaluate_rhs(ENV0, booleantype(B), ENV0, [booleantype(B)]).
evaluate_rhs(ENV0, stringtype(S), ENV0, [stringtype(S)]).
evaluate_rhs(ENV0, numbertype(N), ENV0, [numbertype(N)]).
evaluate_rhs(ENV0, referencetype(Type, Address), ENV0, [referencetype(Type, Address)]).



% Table constructor.
evaluate_rhs(ENV0, tableconstructor(Constructor), ENVn, Result) :-
(
   % Create an empty table.
   Constructor = [] ->
   (
      objectAllocate(ENV0, table([], niltype(nil)), ENVn, Reference),
      Result = [Reference]
   );
   (
      % Create a table from a list of fields.
      Constructor = fields(_) ->
      (
         evaluate_rhs(ENV0, Constructor, ENV1, Map),
         (
            Map = error(_) ->
            Result = Map;
            (
               objectAllocate(ENV1, table(Map, niltype(nil)), ENVn, Reference),
               Result = [Reference]
            )
         )
      );
      (
         % Create a table from a list of expressions.
         Constructor = expressions(_) ->
         (
            evaluate_rhs(ENV0, Constructor, ENV1, Values),
            (
               Values = error(_) ->
               Result = Values;
               (
                  map_build(Values, Map),
                  objectAllocate(ENV1, table(Map, niltype(nil)), ENVn, Reference),
                  Result = [Reference]
               )
            )
         );
         (
            ENVn = ENV0,
            Result = error('Unknown table constructor.')
         )
      )
   )
).



% Enclosed expressions.
evaluate_rhs(ENV0, enclosed(Expression), ENV1, Result) :-
   evaluate_rhs(ENV0, Expression, ENV1, Values),
   (
      member(Values, [error(_), []]) ->
      Result = Values;
      (
         [Value | _] = Values,
         Result = [Value]
      )
   ).



% Evaluate a right-hand side variable.
evaluate_rhs(ENV0, variable(Name), [NewContextPath, NewPool], [Value]) :-
   evaluate_lhs(ENV0, variable(Name), [NewContextPath, NewPool], [ContextPath, Name]),
   getValue(ContextPath, NewPool, Name, Value).



% Evaluate a right-hand side table field accessor.
evaluate_rhs(ENV0, access(E1, E2), ENV1, Result) :-
   evaluate_lhs(ENV0, access(E1, E2), ENV1, Values),
   (
      Values = error(_) ->
      Result = Address;
      (
         Values = [referencetype(_, Address), Key],
         ENV1 = [_, Pool],
         getObject(Pool, Address, table(Map, _)),
         map_get(Map, Key, Value),
         Result = [Value]
      )
   ).



% Evaluate a variadic expression.
evaluate_rhs([ContextPath, Pool], '...', [ContextPath, Pool], Result) :-
   getContext(ContextPath, Pool, context(Map)),
   (
      keyExists(context(Map), '...') ->
      (
         map_get(Map, '...', Values),
         (
            Values = niltype(nil) ->
            Result = [Values];
            Result = Values
         )
      );
      (
         path([_ | Indices]) = ContextPath,
         (
            Indices \= [] ->
            (
               popContext(ContextPath, NewContextPath),
               evaluate_rhs([NewContextPath, Pool], '...', _, Result)
            );
            Result = error('\'...\' is not defined in the execution environment.')
         )
      )
   ).



% The unary minus ('-') operator.
evaluate_rhs(ENV0, unop(unm, Expression), ENVn, Result) :-
   evaluate_rhs(ENV0, Expression, ENV1, Values),
   (
      % If the expression evaluates into an error, then return it.
      Values = error(_) ->
      (
         ENVn = ENV1,
         Result = Values
      );
      (
         % Convert the return value into a number.
         Values = [Value | _],
         evaluate_stat(ENV1, intrinsic(tonumber, Value), ENV2, CTRL, NumericalValues),
         (
            CTRL = error ->
            (
               ENVn = ENV2,
               Result = NumericalValues
            );
            (
               NumericalValues = [Numeric | _],
               (
                  Numeric = numbertype(_) ->
                  (
                     % If the numeric value is numeric, then return its negative value.
                     Numeric = numbertype(N),
                     M is -N,
                     ENVn = ENV2,
                     Result = [numbertype(M)]
                  );
                  (
                     % The value is not numeric then it must be a table.
                     Value \= referencetype(table, _) ->
                     (
                        ENVn = ENV2,
                        Result = error('\'-\' operator requires an operand that is either a number or table.')
                     );
                     (
                        % Call the '__unm' metamethod.
                        Value = referencetype(_, Address),
                        callmetamethod(ENV2, Address, '__unm', [Value], ENVn, Result)
                     )
                  )
               )
            )
         )
      )
   ).



% The not operator.
evaluate_rhs(ENV0, unop(not, Expression), ENV1, Result) :-
   evaluate_rhs(ENV0, Expression, ENV1, Values), !,
   (
      Values = error(_) ->
      Result = Values;
      (
         Values = [Value | _],
         (
            member(Value, [niltype(nil), booleantype(false)]) ->
            Result = [booleantype(true)];
            Result = [booleantype(false)]
         )
      )
   ).



% The length ('#') operator.
evaluate_rhs(ENV0, unop(len, Expression), ENV1, Result) :-
   evaluate_rhs(ENV0, Expression, ENV1, Values), !,
   (
      Values = error(_) ->
      Result = Values;
      (
         Values = [Value | _],
         (
            \+member(Value, [referencetype(table, _), stringtype(_)]) ->
            Result = error('\'#\' operator requires an operand that is either a table or string.');
            (
               Value = referencetype(_, Address) ->
               (
                  % Calculate the length of a table.
                  Value = referencetype(_, Address),
                  ENV1 = [_, Pool],
                  getObject(Pool, Address, table(Map, _)),
                  map_size(Map, Size),
                  Result = [numbertype(Size)]
               );
               (
                  % Calculate the length of a string.
                  Value = stringtype(String),
                  atom_length(String, Length),
                  Result = [numbertype(Length)]
               )
            )
         )
      )
   ).



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
   value_type(V1, T1),
   value_type(V2, T2),
   T1 \= T2, !.
evaluate_rhs(ENV0, binop(eq, E1, E2), ENV2, [booleantype(true)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   value_raw(V1, RV),
   value_raw(V2, RV), !.
evaluate_rhs(ENV0, binop(eq, E1, E2), ENV2, [booleantype(false)]) :-
   evaluate_rhs(ENV0, E1, ENV1, [V1 | _]),
   evaluate_rhs(ENV1, E2, ENV2, [V2 | _]),
   value_raw(V1, RV1),
   value_raw(V2, RV2),
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
   value_type(V1, T1),
   value_type(V2, T2),
   T1 \= T2,
   value_raw(V1, A),
   value_raw(V2, B),
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
evaluate_rhs([ContextPath, Pool], functiondef(PS, SS), ENV, [Reference]) :-
   objectAllocate([ContextPath, Pool], function(PS, SS, ContextPath), ENV, Reference).



% Evaluate a function call.
evaluate_rhs(ENV0, functioncall(E, ES), ENVn, Result) :-
   evaluate_rhs(ENV0, E, ENV1, VS_E),
   evaluate_rhs(ENV1, expressions(ES), ENV2, VS_ES),
   (
      VS_E \= error(_), VS_ES \= error(_) ->
      (
         VS_E \= [referencetype(_, _) | _] ->
         (
            ENVn = ENV1,
            Result = error('Expression is not a callable object.')
         );
         (
            [referencetype(Type, Address) | _] = VS_E,
            (
               Type = table ->
               (
                  % Prepend the calling object to the list of arguments passed to the metamethod.
                  MetamethodArguments = [referencetype(Type, Address) | VS_ES],
                  callmetamethod(ENV2, Address, '__call', MetamethodArguments, ENVn, Result)
               );
               (
                  ENV2 = [ContextPath, Pool],
                  ENVn = [ContextPath, NewPool],
                  getObject(Pool, Address, function(PS, SS, FunctionContextPath)),
                  pushContext([FunctionContextPath, Pool], PS, VS_ES, ENV3), !,
                  evaluate_stat(ENV3, statements(SS), ENV4, _, Result),
                  popContext(ENV4, [_, NewPool])
               )
            )
         )
      );
      (
         VS_E = error(_) ->
         (
            ENVn = ENV1,
            Result = VS_E
         );
         (
            ENVn = ENV2,
            Result = VS_ES
         )
      )
   ).



% Statement evaluation.
% ------------------------------------------------------------------------------

% Evaluate a list of statements.
evaluate_stat(ENV0, statements(Statements), ENVn, CTRL, Result) :-
   (
      Statements = [] ->
      (
         ENVn = ENV0,
         CTRL = continue,
         Result = []
      );
      (
         Statements = [S | SS],
         evaluate_stat(ENV0, S, ENV1, CTRL0, Values),
         (
            \+member(CTRL0, [return, break, error]) ->
            evaluate_stat(ENV1, statements(SS), ENVn, CTRL, Result);
            (
               ENVn = ENV1,
               CTRL = CTRL0,
               Result = Values
            )
         )
      )
   ).



% The assignment statement.
evaluate_stat(ENV0, assign(LHS, RHS), ENVn, CTRL, Result) :-
   evaluate_lhs(ENV0, expressions(LHS), ENV1, Addresses),
   evaluate_rhs(ENV1, expressions(RHS), ENV2, Values),
   (
      Addresses \= error(_), Values \= error(_) ->
      (
         CTRL = continue,
         Result = [],
         setValues(ENV2, Addresses, Values, ENVn)
      );
      (
         (
            Addresses = error(_) ->
            (
               Result = Addresses,
               ENVn = ENV1
            );
            (
               Result = Values,
               ENVn = ENV2
            )
         ),
         CTRL = error
      )
   ).



% The function statement.
evaluate_stat(ENV0, functioncall(E, ES), ENV1, CTRL, Result) :-
   evaluate_rhs(ENV0, functioncall(E, ES), ENV1, Values),
   (
      Values \= error(_) ->
      (
         CTRL = continue,
         Result = []
      );
      (
         CTRL = error,
         Result = Values
      )
   ).



% The do statement.
evaluate_stat(ENV0, do(Statements), ENVn, CTRL, Result) :-
(
   Statements = [] ->
   (
      ENVn = ENV0,
      CTRL = continue,
      Result = []
   );
   (
      pushContext(ENV0, ENV1),
      evaluate_stat(ENV1, statements(Statements), ENV2, CTRL, Result),
      (
         member(CTRL, [return, error]) ->
         ENVn = ENV2;
         popContext(ENV2, ENVn)
      )
   )
).



% The while-do statement.
evaluate_stat(ENV0, while(E, SS), ENVn, CTRL, Result) :-
   evaluate_rhs(ENV0, E, ENV1, VS_E),
   (
      VS_E \= error(_) ->
      (
         VS_E = [Condition | _],
         (
            member(Condition, [niltype(nil), booleantype(false)]) ->
            (
               ENVn = ENV1,
               CTRL = continue,
               Result = []
            );
            (
               evaluate_stat(ENV1, do(SS), ENV2, CTRL0, Values),
               (
                  CTRL0 = break ->
                  (
                     ENVn = ENV2,
                     CTRL = continue,
                     Result = []
                  );
                  (
                     CTRL0 = continue ->
                     evaluate_stat(ENV2, while(E, SS), ENVn, CTRL, Result);
                     (
                        member(CTRL0, [return, error]) ->
                        (
                           ENVn = ENV2,
                           CTRL = CTRL0,
                           Result = Values
                        );
                        true
                     )
                  )
               )
            )
         )
      );
      (
         ENVn = ENV1,
         CTRL = error,
         Result = VS_E
      )
   ).



% The repeat-until statement.
evaluate_stat(ENV0, repeat(E, SS), ENVn, CTRL, Result) :-
   pushContext(ENV0, ENV1),
   evaluate_stat(ENV1, statements(SS), ENV2, CTRL0, VS_SS),
   (
      member(CTRL0, [return, break, error]) ->
      (
         popContext(ENV2, ENVn),
         (
            CTRL0 = break ->
            (
               CTRL = continue,
               Result = []
            );
            (
               CTRL = CTRL0,
               Result = VS_SS
            )
         )
      );
      (
         evaluate_rhs(ENV2, E, ENV3, VS_E),
         (
            VS_E = error(_) ->
            (
               CTRL = error,
               ENVn = ENV3,
               Result = VS_E
            );
            (
               popContext(ENV3, ENV4),
               VS_E = [V | _],
               (
                  member(V, [niltype(nil), booleantype(false)]) ->
                  evaluate_stat(ENV4, repeat(E, SS), ENVn, CTRL, Result);
                  (
                     ENVn = ENV4,
                     CTRL = continue,
                     Result = []
                  )
               )
            )
         )
      )
   ).



% If statement.
evaluate_stat(ENV0, if(Expression, Strue, Sfalse), ENV2, CTRL, Result) :-
   evaluate_rhs(ENV0, Expression, ENV1, Values),
   (
      Values = error(_) ->
      (
         CTRL = error,
         Result = Values
      );
      (
         [Condition | _] = Values,
         (
            \+member(Condition, [niltype(nil), booleantype(false)]) ->
            evaluate_stat(ENV1, Strue, ENV2, CTRL, Result);
            evaluate_stat(ENV1, Sfalse, ENV2, CTRL, Result)
         )
      )
   ).



% Create a variable in the current execution context (scope).
evaluate_stat([ContextPath, Pool], localvariable(Name), ENV, continue, []) :-
   setValue([ContextPath, Pool], ContextPath, Name, niltype(nil), ENV).



% The return statement.
evaluate_stat(ENV0, return(Expressions), ENV1, CTRL, Result) :-
   evaluate_rhs(ENV0, expressions(Expressions), ENV1, Result),
   (
      Result \= error(_) ->
      CTRL = return;
      CTRL = error
   ).



% Break statement.
evaluate_stat(ENV0, break, ENV0, break, []).



% Intrinsic function evaluation.
% ------------------------------------------------------------------------------

% The error function.
evaluate_stat(ENV0, intrinsic(error, E), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, [stringtype(Message) | _]), !.
evaluate_stat(ENV0, intrinsic(error, E), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



% The type function.
evaluate_stat(ENV0, intrinsic(type, E), ENV1, return, [stringtype(Type)]) :-
   evaluate_rhs(ENV0, E, ENV1, [V | _]),
   value_type(V, Type), !.
evaluate_stat(ENV0, intrinsic(type, E), ENV1, error, error(Message)) :-
   evaluate_rhs(ENV0, E, ENV1, error(Message)).



% The print function.
evaluate_stat(ENV0, intrinsic(print, _), ENV0, continue, []).



% The tonumber function.
evaluate_stat(ENV0, intrinsic(tonumber, E), ENV1, CTRL, Result) :-
   evaluate_rhs(ENV0, E, ENV1, Values),
   (
      Values \= error(_) ->
      (
         CTRL = return,
         Values = [Value | _],
         (
            Value = numbertype(_) ->
            Result = [Value];
            (
               Value = stringtype(_) ->
               (
                  Value = stringtype(String),
                  atom_number(String, Number),
                  Result = [numbertype(Number)]
               );
               Result = [niltype(nil)]
            )
         )
      );
      (
         CTRL = error,
         Result = Values
      )
   ).



% The next function.
evaluate_stat(ENV0, intrinsic(next, E1, E2), ENVn, CTRL, Result) :-
   evaluate_rhs(ENV0, E1, ENV1, VS_E1),
   evaluate_rhs(ENV1, E2, ENV2, VS_E2),
   (
      VS_E1 \= error(_), VS_E2 \= error(_) ->
      (
         VS_E1 = [referencetype(table, _) | _] ->
         (
            % Get the table containing the entry to return.
            VS_E1 = [referencetype(_, Address) | _],
            VS_E2 = [Key | _],
            ENV2 = [_, Pool],
            getObject(Pool, Address, table(Map, _)),
            (
               % An empty map has no entries, so it always returns nil.
               Map = [] ->
               Result = [niltype(nil)];
               (
                  % If the key is nil, then the initial table entry is returned.
                  Key = niltype(nil) ->
                  Map = [Result | _];
                  (
                     % If the key doesn't exist, then this is an error.
                     \+map_keyExists(Map, Key) ->
                     Result = error('An invalid key was passed to \'next\'.');
                     (
                        map_next(Map, Key, Entry),
                        (
                           Entry = niltype(nil) ->
                           Result = [Entry];
                           Result = Entry
                        )
                     )
                  )
               )
            ),
            ENVn = ENV2,
            CTRL = return
         );
         (
            CTRL = error,
            ENVn = ENV1,
            Result = error('\'next\' requires a table as its left operand.')
         )
      );
      (
         CTRL = error,
         (
            VS_E1 = error(_) ->
            (
               ENVn = ENV1,
               Result = VS_E1
            );
            (
               ENVn = ENV2,
               Result = VS_E2
            )
         )
      )
   ).



% The pairs function.
evaluate_stat(ENV0, intrinsic(pairs, EXP), ENV1, CTRL, Result) :-
   evaluate_stat(ENV0, return([variable('next'), EXP, niltype(nil)]), ENV1, CTRL, Result).



% The ipairs function.
evaluate_stat(ENV0, intrinsic(ipairs, EXP), ENV1, CTRL, Result) :-
   % Function iterator defintion. Code generated by lua2prolog and formatted for readability.
   IteratorFunctionDefinition = functiondef(
      ['table', 'index'],
      [
         assign([variable('index')], [binop(add, variable('index'), numbertype(1))]),
         localvariable('value'),
         assign([variable('value')], [access(variable('table'), variable('index'))]),
         if(unop(not, binop(eq, variable('value'), niltype(nil))),
            do([return([variable('index'), variable('value')])]),
            do([return([niltype(nil)])])
         )
      ]
   ),
   evaluate_stat(ENV0, return([IteratorFunctionDefinition, EXP, numbertype(0)]), ENV1, CTRL, Result).




% The getmetatable function returns the metatable of a given table, or nil if none is set.
evaluate_stat(ENV0, intrinsic(getmetatable, EXP), ENV1, CTRL, Result) :-
   evaluate_rhs(ENV0, EXP, ENV1, Values),
   (
      Values = [referencetype(table, _) | _] ->
      (
         Values = [referencetype(_, Address) | _],
         ENV1 = [_, Pool],
         getObject(Pool, Address, table(_, MetatableReference)),
         CTRL = return,
         Result = [MetatableReference]
      );
      (
         CTRL = error,
         (
            Values = error(_) ->
            Result = Values;
            Result = error('\'getmetatable\' requires a table as its operand.')
         )
      )
   ).



% The setmetatable function assigns a metatable to a given table.
evaluate_stat(ENV0, intrinsic(setmetatable, EXP1, EXP2), ENVn, CTRL, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   evaluate_rhs(ENV1, EXP2, ENV2, VS_EXP2),
   (
      VS_EXP1 = [referencetype(table, _)], VS_EXP2 = [referencetype(table, _)] ->
      (
         VS_EXP1 = [referencetype(_, TableAddress)],
         VS_EXP2 = [MetatableReference | _],
         ENV2 = [ContextPath, Pool],
         getObject(Pool, TableAddress, table(Map, _)),
         setObject(Pool, TableAddress, table(Map, MetatableReference), NewPool),
         ENVn = [ContextPath, NewPool],
         CTRL = return,
         Result = VS_EXP1
      );
      (
         CTRL = error,
         (
            % If the value returned by either expression is not a reference to a table,
            % then an error is returned. If an error was returned as a result of the
            % evaluation of one of the expressions, then it is returned, otherwise an
            % error signaling an invalid parameter is returned.
            VS_EXP1 = error(_); VS_EXP2 = error(_) ->
            (
               % Evaluation of the first operand raised an error.
               VS_EXP1 = error(_) ->
               (
                  ENVn = ENV1,
                  Result = VS_EXP1
               );
               (
                  % Evaluation of the second operand raised an error.
                  ENVn = ENV2,
                  Result = VS_EXP2
               )
            );
            (
               % The first operand is not a table.
               VS_EXP1 \= referencetype(table, _) ->
               (
                  ENVn = ENV1,
                  Result = error('\'setmetatable\' requires a table as its left operand.')
               );
               (
                  % The second operand is not a table.
                  ENVn = ENV2,
                  Result = error('\'setmetatable\' requires a table as its right operand.')
               )
            )
         )
      )
   ).



% The rawget function returns the value of table[index] without invoking any metamethod.
evaluate_stat(ENV0, intrinsic(rawget, EXP1, EXP2), ENVn, CTRL, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   evaluate_rhs(ENV1, EXP2, ENV2, VS_EXP2),
   (
      VS_EXP1 = [referencetype(table, _) | _], VS_EXP2 \= error(_) ->
      (
         VS_EXP1 = [referencetype(_, Address) | _],
         VS_EXP2 = [Key | _],
         ENV2 = [_, Pool],
         getObject(Pool, Address, table(Map, _)),
         map_get(Map, Key, Value),
         CTRL = return,
         ENVn = ENV2,
         Result = [Value]
      );
      (
         CTRL = error,
         (
            VS_EXP2 = error(_) ->
            (
               ENVn = ENV2,
               Result = VS_EXP2
            );
            (
               ENVn = ENV1,
               (
                  VS_EXP1 = error(_) ->
                  Result = VS_EXP1;
                  Result = error('\'rawget\' requires a table as its left operand.')
               )
            )
         )
      )
   ).



% The rawset function sets the value of table[index] without invoking any metamethod.
evaluate_stat(ENV0, intrinsic(rawset, EXP1, EXP2, EXP3), ENVn, CTRL, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   evaluate_rhs(ENV1, EXP2, ENV2, VS_EXP2),
   evaluate_rhs(ENV2, EXP3, ENV3, VS_EXP3),
   (
      VS_EXP1 = [referencetype(table, _) | _],
      \+member(VS_EXP2, [error(_), [niltype(nil) | _]]),
      VS_EXP3 \= error(_) ->
      (
         VS_EXP1 = [referencetype(_, Address) | _],
         VS_EXP2 = [Key | _],
         VS_EXP3 = [Value | _],

         % Update the table.
         ENV2 = [ContextPath, Pool],
         getObject(Pool, Address, table(OldMap, MetatableAddress)),
         map_set(OldMap, Key, Value, NewMap),
         setObject(Pool, Address, table(NewMap, MetatableAddress), NewPool),
         ENVn = [ContextPath, NewPool],
         CTRL = return,
         Result = VS_EXP1
      );
      (
         CTRL = error,
         (
            % The first operand is not a table.
            VS_EXP1 \= [referencetype(table, _) | _] ->
            (
               ENVn = ENV1,
               (
                  VS_EXP1 = error(_) ->
                  Result = VS_EXP1;
                  Result = error('\'rawset\' requires a table as its first operand.')
               )
            );
            (
               % The third operand is an error.
               VS_EXP3 = error(_) ->
               (
                  ENVn = ENV3,
                  Result = VS_EXP3
               );
               (
                  % The second operand is not an accepted key.
                  ENVn = ENV2,
                  (
                     VS_EXP2 = error(_) ->
                     Result = VS_EXP2;
                     Result = error('\'rawset\' requires a key that is not nil.')
                  )
               )
            )
         )
      )
   ).



% Program evaluation.
% ------------------------------------------------------------------------------
evaluate_chunk([], _, [], []).
evaluate_chunk(Statements, Arguments, Environment, Result, Runtime) :-
   loadEnvironment(Arguments, ENV0),
   get_time(T0),
   evaluate_stat(ENV0, statements(Statements), Environment, _, Result),
   get_time(T1),
   Runtime is (T1 - T0).
