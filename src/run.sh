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

if [ $# -lt 1 ]
then
   echo "Usage: $0 <source> [arguments]"
   echo "$0 requires the source code of a valid Lua program at least."
   echo "Optionally, runtime arguments can be passed to the program."
   exit 1
fi

# Make sure the input is a valid Lua program. This runs the Lua interpreter on
# the input code and checks the exit status upon completion. If the exit status
# is not zero, then the input is not a valid Lua program.
lua "$1" &> /dev/null
if [ $? -gt 0 ]
then
   echo "% Error! '$1' is not a valid Lua program."
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
lua lua2prolog.lua "$1" ${*:2} > $LUA_OUTPUT
swipl -q -f prolua.pl -g main -- $LUA_OUTPUT

exit 0
