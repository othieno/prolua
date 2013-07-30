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

% Run the interpreter. The main/0 predicate loads a file specified in the command
% line arguments after the '--' flag before interpreting it's content. The input
% file should contain a program/1 predicate with it's argument being a list of
% statements (instructions). When the interpreter is done, the program is exited.
main :-
   current_prolog_flag(argv, Arguments),
   append(_, [--, Filename | _], Arguments), !,
   consult(Filename),
   chunk(Statements),
   main(Statements),
   halt.

% Evaluate a list of statements. Before we can evaluate the statements, we load
% the standard, syntax and semantics database, then call the evaluate/5 predicate.
% When evaluation is done, the result is printed as well as the program's last
% memory state, i.e. the state of the environment when the program ended.
main(Statements) :-
   consult('standard.pl'),
   consult('syntax.pl'),
   consult('semantics.pl'),
   % printCallStack(Statements), nl,
   evaluate_p([tabletype([])], [referencetype(1)], Statements, Result, Environment),
   printResult(Result),
   printEnvironment(Environment).

% In case of an evaluation error, print an error message.
main(_) :-
   write('Error! Could not evaluate the program.'), nl.
