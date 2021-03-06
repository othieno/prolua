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

/*
 * Run the interpreter. The main/0 predicate loads a file specified in the command
 * line arguments after the '--' flag, that should contain the chunk/1 and arguments/1
 * predicates. These predicates provide a list of statements and arguments,
 * respectively, which will then be passed onto the main/2 predicate.
 */
main :-
   current_prolog_flag(argv, [Filename | _]),
   consult(Filename),
   chunk(Statements),
   arguments(CommandLineArguments),
   main(Statements, CommandLineArguments),
   halt.

/*
 * Evaluate a list of statements. Before we can evaluate the statements, the
 * 'output' and 'syntax' knowledge bases are loaded and the input program is
 * checked for syntactic correctness. If it is, the 'types', 'environment'
 * and 'semantics' knowledge bases are loaded and the program is evaluated.
 * When evaluation is completed, the execution environment, return values if any,
 * as well as runtime statistics are printed.
 */
main(Statements, CommandLineArguments) :-
   consult('output.pl'),
   consult('syntax.pl'),
   (
      \+statements(Statements) ->
      print_result(error('The program is syntactically incorrect.'));
      (
         consult('types.pl'),
         consult('environment.pl'),
         consult('semantics.pl'),
         evaluate_chunk(Statements, CommandLineArguments, Environment, Result, Runtime),
         print_environment(Environment),
         print_result(Result),
         print_statistics(Runtime)
      )
   ).
