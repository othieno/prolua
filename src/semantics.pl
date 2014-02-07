/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013-2014 Jeremy Othieno.
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



% Expression evaluation.
% ------------------------------------------------------------------------------

% Evaluate a list of left-hand side (LHS) expressions.
evaluate_lhs(ENV0, expressions([]), ENV0, []).
evaluate_lhs(ENV0, expressions([E | ES]), ENVn, Result) :-
   evaluate_lhs(ENV0, E, ENV1, VS_E),
   (
      VS_E \= error(_) ->
      (
         evaluate_lhs(ENV1, expressions(ES), ENVn, VS_ES),
         (
            VS_ES \= error(_) ->
            Result = [VS_E | VS_ES];
            Result = VS_ES
         )
      );
      (
         ENVn = ENV1,
         Result = VS_E
      )
   ).


% Evaluate a left-hand side variable.
evaluate_lhs([ContextPath, Pool], variable(Name), [ContextPath, Pool], [Locator, Name]) :-
   getContext(ContextPath, Pool, Context),
   (
      keyExists(Context, Name) ->
      Locator = ContextPath;
      (
         popContext(ContextPath, path(NewIndices)),
         (
            NewIndices = [] ->
            Locator = ContextPath;
            evaluate_lhs([path(NewIndices), Pool], variable(Name), _, [Locator, _])
         )
      )
   ).



% Evaluate a left-hand side local variable.
evaluate_lhs([ContextPath, Pool], localvariable(Name), [ContextPath, Pool], [ContextPath, Name]).



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
evaluate_rhs(ENV0, access(EXP1, EXP2), ENVn, Result) :-
   evaluate_lhs(ENV0, access(EXP1, EXP2), ENV1, LVALUE),
   (
      LVALUE \= error(_) ->
      (
         ENV1 = [_, Pool],
         LVALUE = [TableReference, Key],
         TableReference = referencetype(_, TableAddress),
         getObject(Pool, TableAddress, table(Map, _)),
         map_get(Map, Key, Value),
         (
            Value \= niltype(nil) ->
            (
               ENVn = ENV1,
               Result = [Value]
            );
            (
               % Find the '__index' metamethod.
               getmetamethod(ENV1, TableReference, '__index', Metamethod),
               (
                  Metamethod = referencetype(table, _) ->
                  evaluate_rhs(ENV1, access(Metamethod, Key), ENVn, Result);
                  (
                     Metamethod = referencetype(function, _) ->
                     evaluate_rhs(ENV1, enclosed(functioncall(Metamethod, [TableReference, Key])), ENVn, Result);
                     (
                        ENVn = ENV1,
                        Result = [niltype(nil)]
                     )
                  )
               )
            )
         )
      );
      (
         ENVn = ENV1,
         Result = LVALUE
      )
   ).



% Evaluate a variadic expression.
evaluate_rhs([ContextPath, Pool], '...', [ContextPath, Pool], Result) :-
   getContext(ContextPath, Pool, context(Map, _)),
   (
      keyExists(context(Map, _), '...') ->
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
                        getmetamethod(ENV2, Value, '__unm', Metamethod),
                        (
                           Metamethod = referencetype(function, _) ->
                           evaluate_rhs(ENV2, enclosed(functioncall(Metamethod, [Value])), ENVn, Result);
                           (
                              ENVn = ENV2,
                              Result = error('The \'__unm\' metamethod is not defined.')
                           )
                        )
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



% Binary arithmetic operators.
evaluate_rhs(ENV0, binop(Operator, E1, E2), ENVn, Result) :-
   member(Operator, [add, sub, mul, div, mod, pow]),
   !,
   evaluate_rhs(ENV0, E1, ENV1, VS_E1),
   evaluate_rhs(ENV1, E2, ENV2, VS_E2),
   (
      VS_E1 \= error(_), VS_E2 \= error(_) ->
      (
         VS_E1 = [V1 | _],
         VS_E2 = [V2 | _],
         evaluate_stat(ENV2, intrinsic(tonumber, V1), ENV3, CTRL1, NV1),
         evaluate_stat(ENV3, intrinsic(tonumber, V2), ENV4, CTRL2, NV2),
         (
            CTRL1 \= error, CTRL2 \= error ->
            (
               % If both operands are numeric, then return the result of a
               % primitive operation between the two numbers.
               NV1 = [numbertype(_) | _], NV2 = [numbertype(_) | _] ->
               (
                  NV1 = [numbertype(A) | _],
                  NV2 = [numbertype(B) | _],
                  arithmetic_op(Operator, A, B, C),
                  ENVn = ENV4,
                  Result = [numbertype(C)]
               );
               (
                  % Find the metamethod to call.
                  atom_concat('__', Operator, MetamethodName),
                  getbinhandler(ENV2, V1, V2, MetamethodName, MetamethodReference),
                  (
                     MetamethodReference = referencetype(function, _) ->
                     evaluate_rhs(ENV2, enclosed(functioncall(MetamethodReference, [V1, V2])), ENVn, Result);
                     (
                        % The metamethod was not defined so we return an error.
                        ENVn = ENV4,
                        format(atom(Message), 'The \'~w\' metamethod is not defined.', [MetamethodName]),
                        Result = error(Message)
                     )
                  )
               )
            );
            (
               % One of the values evaluated into an error.
               CTRL1 = error ->
               (
                  ENVn = ENV3,
                  Result = NV1
               );
               (
                  ENVn = ENV4,
                  Result = NV2
               )
            )
         )
      );
      (
         % One of the expressions evaluated into an error.
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



% The equality operator.
evaluate_rhs(ENV0, binop(eq, EXP1, EXP2), ENVn, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   evaluate_rhs(ENV1, EXP2, ENV2, VS_EXP2),
   (
      VS_EXP1 \= error(_), VS_EXP2 \= error(_) ->
      (
         % Get the raw values and their corresponding types.
         VS_EXP1 = [V_EXP1 | _],
         VS_EXP2 = [V_EXP2 | _],
         V_EXP1 =.. [T1 | RV1],
         V_EXP2 =.. [T2 | RV2],
         (
            % Different types means the expressions are different.
            T1 \= T2 ->
            (
               ENVn = ENV2,
               Result = [booleantype(false)]
            );
            (
               % If the raw values are identical, then the expressions are equivalent.
               RV1 = RV2 ->
               (
                  ENVn = ENV2,
                  Result = [booleantype(true)]
               );
               (
                  % If the values are not equal, then call the '__eq' metamethod.
                  getcomphandler(ENV2, V_EXP1, V_EXP2, '__eq', MetamethodReference),
                  (
                     MetamethodReference = referencetype(function, _) ->
                     evaluate_rhs(ENV2, enclosed(functioncall(MetamethodReference, [V_EXP1, V_EXP2])), ENVn, Result);
                     (
                        ENVn = ENV2,
                        Result = [booleantype(false)]
                     )
                  )
               )
            )
         )
      );
      (
         VS_EXP1 = error(_) ->
         (
            ENVn = ENV1,
            Result = VS_EXP1
         );
         (
            ENVn = ENV2,
            Result = VS_EXP2
         )
      )
   ).



% The less-than operator.
evaluate_rhs(ENV0, binop(lt, EXP1, EXP2), ENVn, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   evaluate_rhs(ENV1, EXP2, ENV2, VS_EXP2),
   (
      VS_EXP1 \= error(_), VS_EXP2 \= error(_) ->
      (
         VS_EXP1 = [V_EXP1 | _],
         VS_EXP2 = [V_EXP2 | _],
         (
            member(V_EXP1, [numbertype(_), stringtype(_), referencetype(table, _)]),
            member(V_EXP2, [numbertype(_), stringtype(_), referencetype(table, _)]) ->
            (
               V_EXP1 = numbertype(_), V_EXP2 = numbertype(_) ->
               (
                  ENVn = ENV2,

                  % Numeric comparison.
                  V_EXP1 = numbertype(A),
                  V_EXP2 = numbertype(B),
                  (
                     A < B ->
                     Result = [booleantype(true)];
                     Result = [booleantype(false)]
                  )
               );
               (
                  V_EXP1 = stringtype(_), V_EXP2 = stringtype(_) ->
                  (
                     ENVn = ENV2,

                     % Lexicographic comparison.
                     V_EXP1 = stringtype(A),
                     V_EXP2 = stringtype(B),
                     (
                        A @< B ->
                        Result = [booleantype(true)];
                        Result = [booleantype(false)]
                     )
                  );
                  (
                     % Metamethod comparison.
                     getcomphandler(ENV2, V_EXP1, V_EXP2, '__lt', MetamethodReference),
                     (
                        MetamethodReference = referencetype(function, _) ->
                        evaluate_rhs(ENV2, enclosed(functioncall(MetamethodReference, [V_EXP1, V_EXP2])), ENVn, Result);
                        (
                           ENVn = ENV2,
                           Result = error('The \'__lt\' metamethod is not defined.')
                        )
                     )
                  )
               )
            );
            (
               % The value is not a number, string or object that may define the '__lt' metamethod.
               \+member(V_EXP1, [numbertype(_), stringtype(_), referencetype(_, _)]) ->
               (
                  ENVn = ENV1,
                  Result = error('The \'<\' operator requires a left operand that is either a number, string or table.')
               );
               (
                  ENVn = ENV2,
                  Result = error('The \'<\' operator requires a right operand that is either a number, string or table.')
               )
            )
         )
      );
      (
         VS_EXP1 = error(_) ->
         (
            ENVn = ENV1,
            Result = VS_EXP1
         );
         (
            ENVn = ENV2,
            Result = VS_EXP2
         )
      )
   ).



% The less-than-or-equal operator.
evaluate_rhs(ENV0, binop(le, EXP1, EXP2), ENVn, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   evaluate_rhs(ENV1, EXP2, ENV2, VS_EXP2),
   (
      VS_EXP1 \= error(_), VS_EXP2 \= error(_) ->
      (
         VS_EXP1 = [V_EXP1 | _],
         VS_EXP2 = [V_EXP2 | _],
         (
            member(V_EXP1, [numbertype(_), stringtype(_), referencetype(table, _)]),
            member(V_EXP2, [numbertype(_), stringtype(_), referencetype(table, _)]) ->
            (
               V_EXP1 = numbertype(_), V_EXP2 = numbertype(_) ->
               (
                  ENVn = ENV2,

                  % Numeric comparison.
                  V_EXP1 = numbertype(A),
                  V_EXP2 = numbertype(B),
                  (
                     A =< B ->
                     Result = [booleantype(true)];
                     Result = [booleantype(false)]
                  )
               );
               (
                  V_EXP1 = stringtype(_), V_EXP2 = stringtype(_) ->
                  (
                     ENVn = ENV2,

                     % Lexicographic comparison.
                     V_EXP1 = stringtype(A),
                     V_EXP2 = stringtype(B),
                     (
                        A @=< B ->
                        Result = [booleantype(true)];
                        Result = [booleantype(false)]
                     )
                  );
                  (
                     % Metamethod comparison.
                     getcomphandler(ENV2, V_EXP1, V_EXP2, '__le', MetamethodReference),
                     (
                        MetamethodReference = referencetype(function, _) ->
                        evaluate_rhs(ENV2, enclosed(functioncall(MetamethodReference, [V_EXP1, V_EXP2])), ENVn, Result);
                        (
                           getcomphandler(ENV2, V_EXP2, V_EXP2, '__lt', BackupMetamethodReference),
                           (
                              BackupMetamethodReference = referencetype(function, _) ->
                              evaluate_rhs(ENV2, unop(not, functioncall(BackupMetamethodReference, [V_EXP2, V_EXP1])), ENVn, Result);
                              (
                                 ENVn = ENV2,
                                 Result = error('The \'__le\' or \'__lt\' metamethod is not defined.')
                              )
                           )
                        )
                     )
                  )
               )
            );
            (
               % The value is not a number, string or object that may define the '__le' metamethod.
               \+member(V_EXP1, [numbertype(_), stringtype(_), referencetype(_, _)]) ->
               (
                  ENVn = ENV1,
                  Result = error('The \'<=\' operator requires a left operand that is either a number, string or table.')
               );
               (
                  ENVn = ENV2,
                  Result = error('The \'<=\' operator requires a right operand that is either a number, string or table.')
               )
            )
         )
      );
      (
         VS_EXP1 = error(_) ->
         (
            ENVn = ENV1,
            Result = VS_EXP1
         );
         (
            ENVn = ENV2,
            Result = VS_EXP2
         )
      )
   ).



% The and operator.
evaluate_rhs(ENV0, binop(and, EXP1, EXP2), ENVn, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   (
      VS_EXP1 \= error(_) ->
      (
         VS_EXP1 = [V_EXP1 | _],
         (
            \+member(V_EXP1, [niltype(nil), booleantype(false)]) ->
            evaluate_rhs(ENV1, EXP2, ENVn, Result);
            (
               ENVn = ENV1,
               Result = [V_EXP1]
            )
         )
      );
      Result = VS_EXP1
   ).



% The or operator.
evaluate_rhs(ENV0, binop(or, EXP1, EXP2), ENVn, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   (
      VS_EXP1 \= error(_) ->
      (
         VS_EXP1 = [V_EXP1 | _],
         (
            member(V_EXP1, [niltype(nil), booleantype(false)]) ->
            evaluate_rhs(ENV1, EXP2, ENVn, Result);
            (
               ENVn = ENV1,
               Result = [V_EXP1]
            )
         )
      );
      Result = VS_EXP1
   ).



% The concatenation operator.
evaluate_rhs(ENV0, binop(concat, EXP1, EXP2), ENVn, Result) :-
   evaluate_rhs(ENV0, EXP1, ENV1, VS_EXP1),
   evaluate_rhs(ENV1, EXP2, ENV2, VS_EXP2),
   (
      VS_EXP1 \= error(_), VS_EXP2 \= error(_) ->
      (
         VS_EXP1 = [V_EXP1 | _],
         VS_EXP2 = [V_EXP2 | _],
         (
            % Make sure the operands are valid.
            member(V_EXP1, [stringtype(_), numbertype(_), referencetype(table, _)]),
            member(V_EXP2, [stringtype(_), numbertype(_), referencetype(table, _)]) ->
            (
               member(V_EXP1, [stringtype(_), numbertype(_)]),
               member(V_EXP2, [stringtype(_), numbertype(_)]) ->
               (
                  ENVn = ENV2,
                  (
                     % If both evaluated expressions belong to the {stringtype, numbertype}
                     % set, perform primitive concatenation.
                     V_EXP1 = numbertype(_), V_EXP2 = numbertype(_) ->
                     (
                        % Number concatenation.
                        V_EXP1 = numbertype(A),
                        V_EXP2 = numbertype(B),
                        atom_length(B, Length),
                        C is ((A * (10 ** Length)) + B),
                        Result = [numbertype(C)]
                     );
                     (
                        % String concatenation.
                        V_EXP1 =.. [_, A],
                        V_EXP2 =.. [_, B],
                        atom_concat(A, B, C),
                        Result = [stringtype(C)]
                     )
                  )
               );
               (
                  % If any of the evaluated values is not a string or number, then
                  % call the "__concat" metamethod if it exists.
                  getbinhandler(ENV2, V_EXP1, V_EXP2, '__concat', MetamethodReference),
                  (
                     MetamethodReference = referencetype(function, _) ->
                     evaluate_rhs(ENV2, enclosed(functioncall(MetamethodReference, [V_EXP1, V_EXP2])), ENVn, Result);
                     (
                        ENVn = ENV2,
                        Result = error('The \'__concat\' metamethod is not defined.')
                     )
                  )
               )
            );
            (
               % An invalid operand was passed to the concatenation operator.
               \+member(V_EXP1, [stringtype(_), numbertype(_), referencetype(table, _)]) ->
               (
                  ENVn = ENV1,
                  Result = error('\'..\' requires a left operand that is either a number, string or table.')
               );
               (
                  ENVn = ENV2,
                  Result = error('\'..\' requires a right operand that is either a number, string or table.')
               )
            )
         )
      );
      (
         % One of the evaluated expressions resulted in an error.
         VS_EXP1 = error(_) ->
         (
            ENVn = ENV1,
            Result = VS_EXP1
         );
         (
            ENVn = ENV2,
            Result = VS_EXP2
         )
      )
   ).



% Evaluate a function definition.
evaluate_rhs([ContextPath, Pool], functiondef(PS, SS), ENV2, [Reference]) :-
   incrementContextLifetime([ContextPath, Pool], ContextPath, ENV1),
   objectAllocate(ENV1, function(PS, SS, ContextPath), ENV2, Reference).



% Evaluate a function call.
evaluate_rhs(ENV0, functioncall(EXP, EXPS), ENVn, Result) :-
   evaluate_rhs(ENV0, EXP, ENV1, VS_EXP),
   evaluate_rhs(ENV1, expressions(EXPS), ENV2, VS_EXPS),
   (
      VS_EXP \= error(_), VS_EXPS \= error(_) ->
      (
         VS_EXP \= [referencetype(_, _) | _] ->
         (
            ENVn = ENV1,
            Result = error('Expression is not a callable object.')
         );
         (
            VS_EXP = [V_EXP  | _],
            V_EXP = referencetype(Type, Address),
            (
               Type = function ->
               (
                  ENV2 = [ContextPath, Pool],
                  ENVn = [ContextPath, NewPool],
                  getObject(Pool, Address, function(PS, SS, FunctionContextPath)),
                  pushContext([FunctionContextPath, Pool], PS, VS_EXPS, ENV3),
                  evaluate_stat(ENV3, statements(SS), ENV4, _, Result),
                  popContext(ENV4, [_, NewPool])
               );
               (
                  % The reference points towards a table. Call the '__call' metamethod.
                  getmetamethod(ENV2, V_EXP, '__call', Metamethod),
                  (
                     Metamethod = referencetype(function, _) ->
                     evaluate_rhs(ENV2, functioncall(Metamethod, [V_EXP | VS_EXPS]), ENVn, Result);
                     (
                        ENVn = ENV2,
                        Result = error('The \'__call\' metamethod is not defined.')
                     )
                  )
               )
            )
         )
      );
      (
         VS_EXP = error(_) ->
         (
            ENVn = ENV1,
            Result = VS_EXP
         );
         (
            ENVn = ENV2,
            Result = VS_EXPS
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
                        % In the case CTRL0 belongs to the {return, error} set...
                        ENVn = ENV2,
                        CTRL = CTRL0,
                        Result = Values
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



% The assert function.
evaluate_stat(ENV0, intrinsic(assert, EXP), ENV1, CTRL, Result) :-
   evaluate_rhs(ENV0, EXP, ENV1, VS_EXP),
   (
      VS_EXP \= error(_) ->
      (
         VS_EXP = [V_EXP | _],
         length(VS_EXP, NumberOfParameters),
         (
            member(V_EXP, [booleantype(false), niltype(nil)]) ->
            (
               % The assertion failed.
               CTRL = error,
               (
                  NumberOfParameters < 2 ->
                  Result = error('Assertion failed.');
                  (
                     % A custom error message was given. Print it.
                     VS_EXP = [_, Message | _],
                     (
                        % If the message is nil, then print the default message.
                        Message = niltype(nil) ->
                        Result = error('Assertion failed.');
                        (
                           % If Message is a function, table or boolean, return a 'bad argument' error.
                           member(Message, [referencetype(_, _), booleantype(_)]) ->
                           (
                              evaluate_stat(ENV1, intrinsic(type, Message), _, _, [stringtype(Type) | _]),
                              format(atom(Output), 'Assertion failed: bad argument #2 to \'assert\' (string expected, got ~w).', Type),
                              Result = error(Output)
                           );
                           (
                              Message =.. [_, ErrorMessage | _],
                              format(atom(Output), 'Assertion failed: ~w', ErrorMessage),
                              Result = error(Output)
                           )
                        )
                     )
                  )
               )
            );
            (
               CTRL = return,
               Result = VS_EXP
            )
         )
      );
      (
         CTRL = error,
         Result = VS_EXP
      )
   ).



% The type function.
evaluate_stat(ENV0, intrinsic(type, EXP), ENV1, CTRL, Result) :-
   evaluate_rhs(ENV0, EXP, ENV1, VS_EXP),
   (
      VS_EXP \= error(_) ->
      (
         % Get the value's internal type (niltype, booleantype, numbertype, stringtype
         % or referencetype) then remove the 'type' suffix from it. Once this is done,
         % return the new type in the form of a string.
         VS_EXP = [V_EXP | _],
         CTRL = return,
         Result = [stringtype(Type)],
         (
            V_EXP = referencetype(_, _) ->
            V_EXP = referencetype(Type, _);
            (
               V_EXP =.. [InternalType | _],
               atom_concat(Type, 'type', InternalType)
            )
         )
      );
      (
         CTRL = error,
         Result = VS_EXP
      )
   ).



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



% The tostring function.
evaluate_stat(ENV0, intrinsic(tostring, E), ENVn, CTRL, Result) :-
   evaluate_rhs(ENV0, E, ENV1, Values),
   (
      Values \= error(_) ->
      (
         Values = [Value | _],
         (
            member(Value, [booleantype(_), numbertype(_), stringtype(_), niltype(nil)]) ->
            (
               Value =.. [_, RawValue],
               ENVn = ENV1,
               CTRL = return,
               Result = [stringtype(RawValue)]
            );
            (
               Value = referencetype(table, _) ->
               (
                  getmetamethod(ENV1, Value, '__tostring', Metamethod),
                  (
                     Metamethod = referencetype(function, _) ->
                     (
                        evaluate_rhs(ENV1, enclosed(functioncall(Metamethod, [Value])), ENVn, Result),
                        (
                           Result = error(_) ->
                           CTRL = error;
                           CTRL = return
                        )
                     );
                     (
                        Value = referencetype(Type, Address),
                        format(atom(Output), '~w:~w', [Type, Address]),
                        ENVn = ENV1,
                        CTRL = return,
                        Result = [stringtype(Output)]
                     )
                  )
               );
               (
                  Value = referencetype(Type, Address),
                  format(atom(Output), '~w:~w', [Type, Address]),
                  ENVn = ENV1,
                  CTRL = return,
                  Result = [stringtype(Output)]
               )
            )
         )
      );
      (
         ENVn = ENV1,
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
         assign([localvariable('value')], [access(variable('table'), variable('index'))]),
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
      VS_EXP1 = [referencetype(table, _) | _], VS_EXP2 = [referencetype(table, _) | _] ->
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
