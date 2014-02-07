PROLUA
======

Prolua is a simple [Lua](http://www.lua.org/) 5.1 interpreter written in [Prolog](http://en.wikipedia.org/wiki/Prolog).
Check out the [__documentation__](https://github.com/supranove/prolua/raw/master/implementation.pdf) [PDF] for more details.


Requirements
------------
Prolua requires [__SWI-Prolog__](http://www.swi-prolog.org/Download.html). Additionally, Prolua requires a Lua
interpreter (lua) and compiler (luac) that implement the [__5.1 specification__](http://www.lua.org/manual/5.1/), a current version of which can be found [__here__](http://www.lua.org/versions.html#5.1).
The interpreter is used to run the __lua2prolog.lua__ script which generates an abstract syntax tree, while the compiler
is used solely to check for any syntax errors in a Lua script.

Prolua has not been tested with Lua programs that adhere to the 5.0 specification or less and as such, I cannot guarantee they
will work in Prolua. Since Prolua currently implements the 5.1 specification, Lua scripts using features introduced in
5.2 and later will not work.


Execution
---------
To run the interpreter, simply execute the __prolua.sh__ script from a terminal. The script takes __one or more arguments__
with the first being the name of the Lua program to interpret, and the rest being command line arguments passed to said program.
An example of execution would be

```bash
./prolua.sh samples/assignment.lua
```

Prolua is developed under Debian GNU/Linux and __the prolua.sh script is written to work with bash__. This means
that it won't work under Windows but should, theoretically, work under any Unix-like operating system (e.g. Mac OS),
provided the stated requirements are met.


Hierarchy
---------
The folders provided with this software are structured in the following manner:
* __samples__ contains sample Lua source code files used to test Prolua.
* __src__ contains the source code tree.
