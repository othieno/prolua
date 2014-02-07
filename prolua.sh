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

# The source directory containing Prolua's source code tree.
CODEBASE="src"

# Check for dependencies.
LUA=$(type -P lua5.1)
if [ ! -x "$LUA" ]
then
   echo "Could not find any executable Lua 5.1 interpreter. Check to make sure it's installed, in the PATH and executable."
   exit 1
fi

LUAC=$(type -P luac)
if [ ! -x "$LUAC" ] || [ -z "$("$LUAC" -v | grep 5.1)" ]
then
   echo "Could not find any executable Lua 5.1 compiler. Check to make sure it's installed, in the PATH and executable."
   exit 1
fi

SWIPL=$(type -P swipl)
if [ ! -x "$SWIPL" ]
then
   echo "Could not find the SWI-Prolog executable. Check to make sure it's installed, in the PATH and executable."
   exit 2
fi

# Collect the Lua script and its command line arguments.
LUA_SCRIPT="$1"
LUA_SCRIPT_ARGS="${*:2}"

# Check for any syntax errors in the Lua program. If the exit status is not zero,
# then the input is assumed to be an invalid script.
$("$LUAC" -p $LUA_SCRIPT) &> /dev/null
if [ $? -gt 0 ]
then
   exit 3
fi

# Generate a random filename for lua2prolog's output. Then define a cleanup function to delete
# it when this bash script is done or interrupted.
LUA2PROLOG_OUTPUT_FILE="/tmp/prolua.$$.output"
function clean()
{
   if [ -e "$LUA2PROLOG_OUTPUT_FILE" ]
   then
      rm "$LUA2PROLOG_OUTPUT_FILE"
   fi
}
trap clean INT TERM EXIT

# Convert the Lua script into Prolog code and store it in the output file, then
# pass the output file to Prolua to be interpreted.
cd "$CODEBASE"
$LUA lua2prolog.lua "../$LUA_SCRIPT" $LUA_SCRIPT_ARGS > $LUA2PROLOG_OUTPUT_FILE
$SWIPL -q -f main.pl -g main -- $LUA2PROLOG_OUTPUT_FILE

exit 0
