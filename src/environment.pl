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


% Load the initial execution environment.
loadEnvironment(Arguments, [ContextPath, Pool]) :-
   Context = context(
   [
      ['...', Arguments],
      ['type', referencetype(function, '0x1')],
      ['print', referencetype(function, '0x2')],
      ['error', referencetype(function, '0x3')],
      ['tonumber', referencetype(function, '0x4')]
   ]),
   ContextPath = path([1]),
   Pool = pool(5,
   [
      ['0x0', 1, graph([node(Context, [])])],
      ['0x1', 1, function(['value'], [intrinsic(type, variable('value'))], ContextPath)],
      ['0x2', 1, function(['output'], [intrinsic(print, variable('output'))], ContextPath)],
      ['0x3', 1, function(['error'], [intrinsic(error, variable('error'))], ContextPath)],
      ['0x4', 1, function(['value'], [intrinsic(tonumber, variable('value'))], ContextPath)]
   ]).



% Push a new context onto the execution context graph.
pushContext(ENV0, ENV1) :-
   pushContext(ENV0, [], [], ENV1).
pushContext(ENV0, Keys, Values, ENV1) :-
   ENV0 = [path(OldIndices), pool(Offset, [[Address, References, OldGraph] | Memory])],
   ENV1 = [path(NewIndices), pool(Offset, [[Address, References, NewGraph] | Memory])],

   % Build the context node.
   map_build(Keys, Values, Map),
   NewNode = node(context(Map), []),

   % Get the new node's parent.
   dag_getNode(OldGraph, path(OldIndices), node(ParentContent, ParentChildren)), !,

   % Make the new node one of the parent's children.
   append(ParentChildren, [NewNode], NewParentChildren),

   % Insert the updated parent node back into the graph.
   dag_setNode(OldGraph, path(OldIndices), node(ParentContent, NewParentChildren), NewGraph),

   % Since we're now in the new node's context, the context path should be updated to
   % reflect this by creating a path to the new node.
   length(NewParentChildren, NewNodeIndex),
   append(OldIndices, [NewNodeIndex], NewIndices), !.



% Remove the last index in the path to the current context.
popContext(path(Indices), path(NewIndices)) :-
(
   Indices = [] ->
   NewIndices = [];
   list_removeLast(Indices, NewIndices)
).
popContext([ContextPath, Pool], [NewContextPath, Pool]) :-
   popContext(ContextPath, NewContextPath).



% Return the context at the given path.
getContext(ContextPath, Pool, Context) :-
   Pool = pool(_, [[_, _, Graph] | _]),
   dag_getNode(Graph, ContextPath, node(Context, _)).



% Does a key exist in a given context?
keyExists(context(Map), Key) :- map_keyExists(Map, Key).



% Return the value with a given key in a given context.
getValue(ContextPath, Pool, Key, Value) :-
   getContext(ContextPath, Pool, context(Map)),
   map_get(Map, Key, Value).



% Set a value in the environment.
setValue([ContextPath, OldPool], Locator, Key, Value, [ContextPath, NewPool]) :-
(
   Locator = path(_) ->
   (
      OldPool = pool(Offset, [[Address, References, OldGraph] | Memory]),
      NewPool = pool(Offset, [[Address, References, NewGraph] | Memory]),
      dag_getNode(OldGraph, Locator, node(context(Map), Children)),
      map_set(Map, Key, Value, NewMap),
      dag_setNode(OldGraph, Locator, node(context(NewMap), Children), NewGraph)
   );
   (
      Locator = referencetype(_, Address),
      getObject(OldPool, Address, table(Map)),
      map_set(Map, Key, Value, NewMap),
      setObject(OldPool, Address, table(NewMap), NewPool)
   )
).




% Set multiple values in the environment.
setValues(ENV0, [], _, ENV0).
setValues(ENV0, [[Locator, Key] | Addresses], Values, ENVn) :-
(
   Values = [] ->
   (
      setValue(ENV0, Locator, Key, niltype(nil), ENV1),
      setValues(ENV1, Addresses, [], ENVn)
   );
   (
      [Value | MoreValues] = Values,
      setValue(ENV0, Locator, Key, Value, ENV1),
      setValues(ENV1, Addresses, MoreValues, ENVn)
   )
).




% Add an object to the memory pool and return a reference to it.
objectAllocate(pool(OFF0, MEM0), Object, pool(OFF1, MEM1), referencetype(Type, Address)) :-
   hex(OFF0, Address),
   OFF1 is OFF0 + 1,
   append(MEM0, [[Address, 1, Object]], MEM1),
   (
      Object = table(_) ->
      Type = table;
      Type = function
   ).
objectAllocate([ContextPath, Pool], Object, [ContextPath, NewPool], Reference) :-
   objectAllocate(Pool, Object, NewPool, Reference).



% Delete an object from the memory pool at the given address.
objectDelete(pool(Offset, Memory), Address, pool(Offset, NewMemory)) :-
   delete(Memory, [Address, _, _], NewMemory), !.
objectDelete([ContextPath, Pool], Address, [ContextPath, NewPool]) :-
   objectDelete(Pool, Address, NewPool).



% Return an object at the given address in the memory pool.
getObject(pool(_, [[MemoryAddress, _, Object] | Memory]), Address, Result) :-
(
   MemoryAddress = Address ->
   Result = Object;
   (
      Memory = [] ->
      Result = niltype(nil);
      getObject(pool(_, Memory), Address, Result)
   )
).


% Set the value of an object at the given pool address.
setObject(pool(Offset, Memory), Address, Value, pool(Offset, NewMemory)) :-
(
   Memory = [] ->
   NewMemory = [];
   (
      Memory = [[MemoryAddress, References, MemoryBlock] | MemoryBlocks],
      (
         MemoryAddress = Address ->
         NewMemory = [[MemoryAddress, References, Value] | MemoryBlocks];
         (
            setObject(pool(Offset, MemoryBlocks), Address, Value, pool(Offset, NewMemoryBlocks)),
            NewMemory = [[MemoryAddress, References, MemoryBlock] | NewMemoryBlocks]
         )
      )
   )
).
