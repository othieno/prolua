-- The MIT License (MIT)
--
-- Copyright (c) 2013 Jeremy Othieno.
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

-- lua2prolog is inspired by https://github.com/davidm/lua2c/blob/master/lua2c.lua
-- written by David Manura. It will convert the abstract syntax tree of a Lua
-- program into a Prolog format that can be interpreted by Prolua.

local _G      = _G
local assert  = _G.assert
local error   = _G.error
local io      = _G.io
local ipairs  = _G.ipairs
local os      = _G.os
local package = _G.package
local require = _G.require
local string  = _G.string
local table   = _G.table

-- Include parsing libraries taken from Metalua.
package.path = package.path .. ";./metalua/?.lua"

require "lexer"
require "gg"
require "mlp_lexer"
require "mlp_misc"
require "mlp_table"
require "mlp_meta"
require "mlp_expr"
require "mlp_stat"
require "mlp_ext"
_G.mlc = {} -- make gg happy
local mlp = assert(_G.mlp)

-- Load input file.
local filename = ...;
if not filename then
  io.stderr:write("usage: lua2prolog filename.lua\n");
  os.exit(1);
end
local file = assert(io.open (filename, 'r'));
local input = file:read "*a";
file:close();
input = input:gsub("^#[^\r\n]*", "") -- remove any shebang

-- Get the program's abstract syntax tree (AST).
local lx = mlp.lexer:newstream(input);
local AST = mlp.chunk(lx);

-- This function converts an AST's node into Prolog, in a format that
-- can be parsed and interpreted by Prolua.
function ASTNodeToProlog(ASTNode)
	local Prolog = "s0, s1, s2, s3, sn";
	return Prolog;
end

-- Recursively convert the root node into Prolog.
print("program(["..ASTNodeToProlog(AST).."]).");
