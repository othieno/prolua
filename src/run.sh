#!/bin/bash
#
# The MIT License (MIT)
#
# Copyright (c) 2013 Jeremy Othieno.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

if [ $# -ne 1 ]
then
   echo "$0 requires the source code of a lua program, e.g. $0 helloworld.lua"
   exit 1
fi

# A randomly generated name for our output file and a cleanup function to delete
# it when the script is done.
LUA_OUTPUT="/tmp/prolua.$$.output"
function clean()
{
	if [[ -e "$LUA_OUTPUT" ]]
	then
		rm "$LUA_OUTPUT"
	fi
}
trap clean INT TERM EXIT

# Convert the Lua source code into prolog and store it in the output file, then
# pass the output file to prolua to be interpreted.
lua lua2prolog.lua "$1" > $LUA_OUTPUT
swipl -q -f prolua.pl -g main -- $LUA_OUTPUT

exit 0
