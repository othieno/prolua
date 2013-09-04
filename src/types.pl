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


% List manipulation.
% ------------------------------------------------------------------------------

% Replace the nth element (starting from 1) of the list.
list_replace([], 1, Element, [Element]).
list_replace([], N, _, []) :- N > 1.
list_replace([_ | Tail], 1, Element, [Element | Tail]).
list_replace([Head | Tail], N, Element, [Head | NewTail]) :-
   N > 1,
   M is N - 1,
   list_replace(Tail, M, Element, NewTail).


% Remove the last element in the list
list_removeLast(List, NewList) :-
   append(NewList, [_], List).




% Map manipulation.
% ------------------------------------------------------------------------------

% Get a value in the map.
map_get([], _, niltype(nil)).
map_get([[K, V] | _], K, V).
map_get([[K, _] | M], K1, V) :-
   K \== K1,
   map_get(M, K1, V).


% Set a value in the map.
map_set([], K, V, [[K, V]]).
map_set([[K, _] | M], K, V, [[K, V] | M]).
map_set([[K, V] | M], K1, V1, [[K, V] | M1]) :-
   K \== K1,
   map_set(M, K1, V1, M1).


% Create a map from a list of keys and values. Be wary of varargs.
map_build([], _, []).
map_build(['...' | _], VS, [['...', VS]]).
map_build([K | KS], [V | VS], M1) :-
   K \== '...',
   map_build(KS, VS, M),
   append([[K, V]], M, M1).
map_build([K | KS], [], M1) :-
   K \== '...',
   map_build(KS, [], M),
   append([[K, niltype(nil)]], M, M1).

% Create a map from a list of values only. The keys are generated.
map_build([], []).
map_build(VS, M) :-
   length(VS, Length),
   map_generateKeys(1, Length, KS),
   map_build(KS, VS, M).

% Generate a list of integer keys from I to (I + N).
map_generateKeys(_, 0, []).
map_generateKeys(I, N, [numbertype(I) | KS]) :-
   I > 0,
   J is I + 1,
   M is N - 1,
   map_generateKeys(J, M, KS).

% Does a given key exist in the map?
map_keyExists([], _) :- false.
map_keyExists([[K, _] | _], K).
map_keyExists([[K, _] | M], K1) :-
   K \== K1,
   map_keyExists(M, K1).


% Return the number of entries in the map.
map_size([], 0).
map_size([[_, _] | M], Size) :-
   map_size(M, SubmapSize),
   Size is SubmapSize + 1.



% Directed acyclic graph (DAG) data structure.
% ------------------------------------------------------------------------------

% Return a node at the given path in a graph.
dag_getNode(graph(RootNodes), path([NodeIndex | Indices]), Node) :-
(
   Indices = [] ->
   nth1(NodeIndex, RootNodes, Node);
   (
      nth1(NodeIndex, RootNodes, node(_, Subgraph)),
      dag_getNode(graph(Subgraph), path(Indices), Node)
   )
).


% Set the value of a node.
dag_setNode(graph(RootNodes), path([NodeIndex | Indices]), NewNode, graph(NewRootNodes)) :-
(
   Indices = [] ->
   list_replace(RootNodes, NodeIndex, NewNode, NewRootNodes);
   (
      nth1(NodeIndex, RootNodes, node(Content, Subgraph)),
      dag_setNode(graph(Subgraph), path(Indices), NewNode, graph(NewSubgraph)),
      list_replace(RootNodes, NodeIndex, node(Content, NewSubgraph), NewRootNodes)
   )
).


% Count the number of nodes in the graph.                                                             FIXME
dag_countNodes(graph(RootNodes), Count) :-
(
   RootNodes = [] ->
   Count = 0;
   (
      [Node | Nodes] = RootNodes,
      dag_countNodes(Node, A),
      dag_countNodes(graph(Nodes), B),
      Count is 1000 + A + B
   )
).
dag_countNodes(node(_, _), 1).
