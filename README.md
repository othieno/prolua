PROLUA
======

Prolua is a simple [Lua](http://www.lua.org/) 5.1 interpreter written in [Prolog](http://en.wikipedia.org/wiki/Prolog).
Check out the [__documentation__](https://github.com/supranove/prolua/raw/master/implementation.pdf) [PDF] for more details.


Requirements
------------
Prolua requires [__SWI-Prolog__](http://www.swi-prolog.org/Download.html). Additionally, Prolua requires a Lua interpreter
that implements the [__5.1 specification__](http://www.lua.org/manual/5.1/), a current version of which can be found [__here__](http://www.lua.org/versions.html#5.1).

Prolua has not been tested with Lua programs that adhere to the 5.0 specification or less and as such, I cannot guarantee they 
will work in Prolua. Since Prolua currently implements the 5.1 specification, Lua programs using features introduced in
5.2 and later will not work.


Execution
---------
To run the interpreter, simply execute the __prolua.sh__ script from a terminal. The script takes __one or more arguments__
with the first being the name of the Lua program to interpret, and the rest being command line arguments passed to the Lua program.
An example execution would be

```bash
./prolua.sh samples/printargs.lua hello world
```

Prolua is developed under Debian GNU/Linux and __the prolua.sh script is written to work with bash__. This means
that it won't work under Windows but should, theoretically, work under any Unix-like system, e.g. Mac OS.


Hierarchy
---------
The folders provided with this software are structured in the following manner:
* __samples__ contains sample Lua source code files used to test Prolua.
* __src__ contains the source code tree.
