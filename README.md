PROLUA
======

Prolua is a simple [Lua](http://www.lua.org/) interpreter written in [Prolog](http://en.wikipedia.org/wiki/Prolog).
Check out the [__documentation__](https://github.com/supranove/prolua/raw/master/implementation.pdf) for more details.


Requirements
------------
Prolua requires [SWI-Prolog](http://www.swi-prolog.org/Download.html) to be able to execute
the interpreter, and an installation of [Lua 5.1 or better](http://www.lua.org/download.html) to run __lua2prolog.lua__, which
converts Lua source code into Prolog.

__lua2prolog__ uses a few parsing libraries taken from the [Metalua 0.4.1 RC1](http://metalua.luaforge.net/) compiler written and
[__licensed__](https://github.com/supranove/prolua/blob/master/src/metalua/LICENSE.md) by Fabien Fleutot.
These files can be found [here](https://github.com/supranove/prolua/blob/master/src/metalua/).


Execution
---------
To run the interpreter, simply execute the __run.sh__ script from a terminal. The script takes
__one argument__ which is name of the file containing the Lua source code to interpret.
An execution would look like

```bash
./run.sh helloworld.lua
```

Note that this code is tested under Debian GNU/Linux and __the run.sh script was written to work
with bash__. This means that it won't work under Windows but should, theoretically, work under Mac OS.


Layout
------
The folders provided with this software are structured in the following manner
* __samples__ contains sample Lua source code files used to test the interpreter.
* __src__ contains the source code tree.
