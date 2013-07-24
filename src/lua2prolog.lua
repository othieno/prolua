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

-- An associative array containing references to the functions that'll convert
-- a specific type of node into Prolog, based on its tag.
convert = {}

-- Convert a nil value into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'niltype(nil)'.
convert["Nil"] = function(ASTNode)
   return "niltype(nil)"
end

-- Convert a boolean true value into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'booleantype(true)'.
convert["True"] = function(ASTNode)
   return "booleantype(true)"
end

-- Convert a boolean false value into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'booleantype(false)'.
convert["False"] = function(ASTNode)
   return "booleantype(false)"
end

-- Convert a number value into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'numbertype(n)', where n is a real number (mathematically).
convert["Number"] = function(ASTNode)
   return "numbertype(" .. ASTNode[1] .. ")"
end

-- Convert a string value into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'stringtype(s)', where 's' is a single quoted ('') string.
-- 's' is also formatted to handle escape characters.
convert["String"] = function(ASTNode)
   return "stringtype('" .. string.gsub(ASTNode[1], "'", "''") .. "')"
end

-- Convert a function definition node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'functionbody(ps, b)' where ps is a list of parameters
-- and b is the function's instruction block.
convert["Function"] = function(ASTNode)
   -- Create function parameters.
   local parameters = ""
   local nParameters = #ASTNode[1]
   if (nParameters > 0) then
      for i = 1, nParameters do
         parameters = parameters .. ASTNodeToProlog(ASTNode[i])
         if (i < nParameters) then
            parameters = parameters .. ", "
         end
      end
   end
   return "functionbody([" .. parameters .. "], block([" .. ASTNodeToProlog(ASTNode[2]) .. "]))"
end

-- Convert a table type node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'tabletype(t)', where t is a table value.
convert["Table"] = function(ASTNode)
   -- Create table entries while taking implicit keys into account.
   local nextImplicitKey = 1
   local output = ""
   for i = 1, #ASTNode do
      -- If the entry is a pair, then it already has a key.
      local hasKey = (ASTNode[i].tag == "Pair")
      if (hasKey) then
         output = output .. ASTNodeToProlog(ASTNode[i])
      else
         output = output .. "[numbertype(" .. nextImplicitKey .. "), " .. ASTNodeToProlog(ASTNode[i]) .. "]"
         nextImplicitKey = nextImplicitKey + 1
      end
      if (i < #ASTNode) then
         output = output .. ", "
      end
   end
   return "tabletype([" .. output .. "])"
end

-- Convert a pair into Prolog.
-- param ASTNode the node to convert.
-- Returns the string '[a, b]' where a and b are expressions.
convert["Pair"] = function(ASTNode)
   return "[" .. ASTNodeToProlog(ASTNode[1]) .. ", " .. ASTNodeToProlog(ASTNode[2]) .. "]"
end

-- Convert a parenthesised expression into Prolog.
-- param ASTNode the node to convert.
-- Let the input be a node representing '(e)'. This function returns the expression e
-- converted into Prolog, thereby discarding the parentheses.
convert["Paren"] = function(ASTNode)
   return ASTNodeToProlog(ASTNode[1])
end

-- Convert a 'do .. end' node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'do(b)' where b is an instruction block.
convert["Do"] = function(ASTNode)
   local block = ""
   for i = 1, #ASTNode do
      block = block .. ASTNodeToProlog(ASTNode[i])
      if (i < #ASTNode) then
         block = block .. ", "
      end
   end
   return "do(block([" .. block .. "]))"
end

-- Convert an 'if' node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'if(c, btrue, bfalse)' where c is the condition, btrue the
-- instruction block that is run if the condition is true, and bfalse if it is false.
-- In the case of an 'if .. elseif', the returned string will be
-- 'if(c1, btrue1, if(c2, btrue2, emptyblock))' and an 'if .. elseif .. else' statement
-- will produce 'if(c1, btrue1, if(c2, btrue2, bfalse))'
convert["If"] = function(ASTNode)
   -- Convert condition-body pairs.
   local output = ""
   for i = 1, #ASTNode - 1, 2 do
      output =
      output .. "if(" .. ASTNodeToProlog(ASTNode[i]) ..
      ", block([" .. ASTNodeToProlog(ASTNode[i + 1]) .. "]), "
   end
   -- Create bfalse.
   local bfalse = "block([" .. ASTNodeToProlog(ASTNode[#ASTNode]) .. "])"

   return output .. bfalse .. string.rep(")", #ASTNode/2)
end

-- Convert an assignment node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'assign(LHS, RHS)', where LHS and RHS are a list of
-- left and right hand side expressions, respectively.
convert["Set"] = function(ASTNode)
   local output = ""

   -- Create the left-hand side expressions.
   for i = 1, #ASTNode[1] do
      output = output .. ASTNodeToProlog(ASTNode[1][i])
      if (i < #ASTNode[1]) then
         output = output .. ", "
      end
   end

   output = output .. "], ["

   -- Create the right-hand side expressions.
   for i = 1, #ASTNode[2] do
      output = output .. ASTNodeToProlog(ASTNode[2][i])
      if (i < #ASTNode[2]) then
         output = output .. ", "
      end
   end
   return "assign([" .. output .. "])"
end

-- Convert an identifier (variable) node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'variable(n)', where n is the variable name.
convert["Id"] = function(ASTNode)
   return "variable('" .. ASTNode[1] .. "')"
end

-- Convert a local variable node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'localvariable(n, v)', where n is the variable name and v is
-- the variable's initial value; or 'localvariable(n)', where no initial value is
-- specified, in which case it is implicitly set to 'nil'.
convert["Local"] = function(ASTNode)
   local output = ""

   -- The number of declared variables and values.
   local nDeclaredVariables = #ASTNode[1]
   local nDeclaredvalues = #ASTNode[2]

   -- Get the variables.
   for i = 1, nDeclaredVariables do
      output = output .. "localvariable('" .. ((ASTNode[1])[i])[1] .. "'"

      -- Does the variable have an initial value?
      if (i <= nDeclaredvalues) then
         output = output .. ", " .. ASTNodeToProlog((ASTNode[2])[i])
      end
      -- Close the parenthesis.
      if (i < nDeclaredVariables) then
         output = output .. "), "
      else
         output = output .. ")"
      end
   end
   return output
end

-- Convert an operator node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'unop(n, e)' in the case of a unary operator or
-- 'binop(n, elhs, erhs)' in the case of a binary operator, where n is the name
-- of the operator, and e, elhs and erhs are expressions.
convert["Op"] = function(ASTNode)
   -- Get the operator name.
   local name = ASTNode[1]

   -- Get the operator's parity. Once the parity has been established, change the
   -- operator name to match the documented syntax (see documentation) if need be.
   local parity = ""
   local nOperands = #ASTNode - 1
   if (nOperands == 1) then
      parity = "unop"

      if     (name == "unm") then name = "negative"
      elseif (name == "not") then name = "not"
      elseif (name == "len") then name = "length"
      end
   else
      parity = "binop"

      if  (name == "concat") then name = "concatenate"
      elseif (name ==  "eq") then name = "equal"
      elseif (name == "sub") then name = "subtract"
      elseif (name == "mul") then name = "multiply"
      elseif (name == "div") then name = "divide"
      elseif (name == "pow") then name = "exponent"
      elseif (name == "mod") then name = "modulo"
      end
   end

   -- Convert the operands.
   local operands = ""
   for i = 2, #ASTNode do
      operands = operands .. ASTNodeToProlog(ASTNode[i])
      if (i < #ASTNode) then
         operands = operands .. ", "
      end
   end
   return parity .. "(" .. name .. ", " .. operands .. ")"
end

-- Convert a function call node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'functioncall(v, ps)' where v is the variable referencing the function
-- and ps is a list of function arguments.
convert["Call"] = function(ASTNode)
   -- Create function arguments.
   local arguments = ""
   for i = 2, #ASTNode do
      arguments = arguments .. ASTNodeToProlog(ASTNode[i])
      if (i < #ASTNode) then
         arguments = arguments .. ", "
      end
   end
   return "functioncall(" .. ASTNodeToProlog(ASTNode[1]) .. ", [" .. arguments .. "])"
end

-- Convert a return node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'return(es)' where es is a list of expressions.
convert["Return"] = function(ASTNode)
   local expressions = ""

   for i = 1, #ASTNode do
      expressions = expressions .. ASTNodeToProlog(ASTNode[i])
      if (i < #ASTNode) then
         expressions = expressions .. ", "
      end
   end
   return "return([" .. expressions .. "])"
end

-- Convert a table access node into Prolog.
-- param ASTNode the node to be converted.
-- Returns the string 'access(t, k)' where t is an expression that evaluates into a
-- table and k is an expression that evaluates into a table key.
convert["Index"] = function(ASTNode)
   return "access(" .. ASTNodeToProlog(ASTNode[1]) .. ", " .. ASTNodeToProlog(ASTNode[2]) .. ")"
end

-- Convert a while loop node into Prolog.
-- param ASTNode the node to be converted.
-- Returns the string 'while(e, b)' where e is an expression that evaluates into a loop
-- condition and b is the instruction block that is executed while the condition is true.
convert["While"] = function(ASTNode)
   return "while(" .. ASTNodeToProlog(ASTNode[1]) .. ", block([" .. ASTNodeToProlog(ASTNode[2]) .. "]))"
end

-- Convert a repeat-until loop node into Prolog.
-- param ASTNode the node to be converted.
-- Returns the string 'repeat(e, b)' where e is an expression that evaluates into a
-- condition and b is the instruction block that is executed until the condition is met.
convert["Repeat"] = function(ASTNode)
   return "repeat(" .. ASTNodeToProlog(ASTNode[2]) .. ", block([" .. ASTNodeToProlog(ASTNode[1]) .. "]))"
end

-- Convert a numerical for loop node into Prolog.
-- param ASTNode the node to be converted.
-- Returns the string 'for(v, i, e, s, b)' where v is the count variable, i is an
-- expression that evaluates into a numbertype and which will be the count variable's
-- initial value, e an expression that evaluates into a numbertype and will be the
-- count variable's stop value, s an expression that evaluates into a numbertype
-- which determines the count variable's increment value, and b the instruction block
-- that is executed while the count variable has not reached it's end value.
convert["Fornum"] = function(ASTNode)
   -- Get the variable, and the initial and end values.
   local variable = ASTNodeToProlog(ASTNode[1])
   local start = ASTNodeToProlog(ASTNode[2])
   local stop = ASTNodeToProlog(ASTNode[3])

   -- Get the increment value. If this value is not specified, then
   -- it is implicitly set to numbertype(1).
   local increment = "numbertype(1)"
   local nodeLength = #ASTNode
   if (nodeLength > 4) then
      increment = ASTNodeToProlog(ASTNode[4])
   end

   -- Get the instruction block.
   local block = "block([" .. ASTNodeToProlog(ASTNode[nodeLength]) .. "])"

   return
   "for(" .. variable .. ", " .. start .. ", " .. stop .. ", " .. increment .. ", " .. block .. ")"
end

-- ???
convert["Invoke"] = function(ASTNode)
   return "'TO::IMPLEMENT::INVOKE'"
end

-- ???
convert["Dots"] = function(ASTNode)
   return "'TO::IMPLEMENT::DOTS'"
end

-- This function converts an AST's node into Prolog, in a format that
-- can be parsed and interpreted by Prolua.
-- param ASTNode the abstract syntax tree node to convert.
function ASTNodeToProlog(ASTNode)
   -- If the tag is nil, then the node needs to be processed recursively.
   if (ASTNode.tag == nil) then
      local output = ""
      for i = 1, #ASTNode do
         output = output .. ASTNodeToProlog(ASTNode[i])
         if (i < #ASTNode) then
            output = output .. ", "
         end
      end
      return output
   else
      return convert[ASTNode.tag](ASTNode)
   end
end

-- Load input file.
local filename = ...
if not filename then
   io.stderr:write("usage: lua2prolog filename.lua\n")
   os.exit(1)
end
local file = assert(io.open (filename, 'r'))
local input = file:read "*a"
file:close()
input = input:gsub("^#[^\r\n]*", "") -- remove any shebang

-- Get the program's abstract syntax tree and recursively convert it into Prolog.
_G.mlc = {} -- make gg happy
local mlp = assert(_G.mlp)
print("program([" .. ASTNodeToProlog(mlp.chunk(mlp.lexer:newstream(input))) .. "]).")
