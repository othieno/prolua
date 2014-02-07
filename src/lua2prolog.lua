-- The MIT License (MIT)
--
-- Copyright (c) 2013-2014 Jeremy Othieno.
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

-- Convert a given string s to 'stringtype('s')', formatting s if need be.
function toStringType(string)
   local formattedOutput = string.format("%q", string)

   -- Remove quotation.
   formattedOutput = formattedOutput:sub(2, string.len(formattedOutput) - 1)

   formattedOutput = formattedOutput
   :gsub("\a", "\\a")
   :gsub("\b", "\\b")
   :gsub("\f", "\\f")
   :gsub("\n", "\\n")
   :gsub("\r", "\\r")
   :gsub("\t", "\\t")
   :gsub("\v", "\\v")
   :gsub("'", "''")

   return "stringtype('" .. formattedOutput .. "')"
end

-- Convert a string value into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'stringtype(s)', where 's' is a single quoted ('') string.
-- 's' is also formatted to handle escape characters.
convert["String"] = function(ASTNode)
   return toStringType(ASTNode[1])
end

-- Convert a function definition node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'functiondef(ps, ss)' where ps is a list of parameters
-- and ss is the function's statement block.
convert["Function"] = function(ASTNode)
   -- Create function parameters.
   local parameters = ""
   local nParameters = #ASTNode[1]
   if (nParameters > 0) then
      for i = 1, nParameters do
         local node = ASTNode[1][i]
         if (node.tag == "Id") then
            parameters = parameters .. "'" .. node[1] .. "'"
         else
            parameters = parameters .. ASTNodeToProlog(node)
         end
         if (i < nParameters) then
            parameters = parameters .. ", "
         end
      end
   end
   return "functiondef([" .. parameters .. "], [" .. ASTNodeToProlog(ASTNode[2]) .. "])"
end

-- Convert a table constructor node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'tableconstructor(E)', where E is an expression that's
-- used to create the table.
convert["Table"] = function(ASTNode)
   local nElements = #ASTNode

   -- If there're no elements in the table, return an empty table constructor.
   if nElements == 0 then
      return "tableconstructor([])"
   end

   -- Check if the field list has no explicit keys, i.e. it's a list of expressions.
   local isExplist = true
   for i = 1, nElements do
      -- If the entry is a pair, then a key has been set.
      if ASTNode[i].tag == "Pair" then
         isExplist = false
         break
      end
   end

   if isExplist then
      -- If we have a list of expressions, then we don't need to calculate implicit keys.
      local output = ""
      for i = 1, nElements do
         output = output .. ASTNodeToProlog(ASTNode[i]) .. ", "
      end

      -- Remove trailing ", " and return the output.
      return "tableconstructor(expressions([" .. output:sub(1, string.len(output) - 2) .. "]))"
   else
      -- On the other hand, if a key is set for at least one table entry, then implicit
      -- keys need to be calculated.
      local nextImplicitKey = 1
      local output = ""

      for i = 1, #ASTNode do
         -- If the node is a pair, then it has a key.
         local hasKey = (ASTNode[i].tag == "Pair")
         if hasKey then
            output = output .. ASTNodeToProlog(ASTNode[i])
         else
            -- Add an implicit key otherwise.
            output = output .. "[numbertype(" .. nextImplicitKey .. "), " .. ASTNodeToProlog(ASTNode[i]) .. "]"
            nextImplicitKey = nextImplicitKey + 1
         end
         if i < #ASTNode then
            output = output .. ", "
         end
      end
      return "tableconstructor(fields([" .. output .. "]))"
   end
end

-- Convert a variadic expression node into Prolog.
-- param ASTNode the node to be converted.
-- Returns the string '...'.
convert["Dots"] = function(ASTNode)
   return "'...'"
end

-- Convert a pair node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string '[e1, e2]' where e1 and e2 are expressions.
convert["Pair"] = function(ASTNode)
   return "[" .. ASTNodeToProlog(ASTNode[1]) .. ", " .. ASTNodeToProlog(ASTNode[2]) .. "]"
end

-- Convert a parenthesised expression into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'enclosed(e)' where e is an expression.
convert["Paren"] = function(ASTNode)
   return "enclosed(" .. ASTNodeToProlog(ASTNode[1]) .. ")"
end

-- Convert a 'do .. end' statement node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'do(ss)' where ss is a statement block.
convert["Do"] = function(ASTNode)
   local block = ""
   for i = 1, #ASTNode do
      block = block .. ASTNodeToProlog(ASTNode[i])
      if (i < #ASTNode) then
         block = block .. ", "
      end
   end
   return "do([" .. block .. "])"
end

-- Convert an 'if' node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'if(c, sstrue, ssfalse)' where c is the condition, sstrue the
-- statement block that is run if the condition is true, and ssfalse if it is false.
-- In the case of an 'if .. elseif', the returned string will be
-- 'if(c1, sstrue1, if(c2, sstrue2, do([])))' and an 'if .. elseif .. else' statement
-- will produce 'if(c1, sstrue1, if(c2, sstrue2, ssfalse))'
convert["If"] = function(ASTNode)
   -- Convert condition-body pairs.
   local output = ""
   for i = 1, #ASTNode - 1, 2 do
      output =
      output .. "if(" .. ASTNodeToProlog(ASTNode[i]) ..
      ", do([" .. ASTNodeToProlog(ASTNode[i + 1]) .. "]), "
   end
   -- Create bfalse iff there is one.
   local bfalse = ""
   if (#ASTNode > 2) then
      bfalse = ASTNodeToProlog(ASTNode[#ASTNode])
   end
   return output .. "do([" .. bfalse .. "])" .. string.rep(")", #ASTNode/2)
end

-- Convert an assignment statement node into Prolog.
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
-- Declaring a local variable is a semantic sugar for an assignment of variables
-- in the current scope. For example, the statement "local x, y, z = 1, 2, 3" will
-- generate the following code:
-- "assign([localvariable('x'), localvariable('y'), localvariable('z')],
--         [numbertype(1), numbertype(2), numbertype(3)])"
convert["Local"] = function(ASTNode)
   local assign = "assign(["

   -- Get LHS expressions.
   for i = 1, #ASTNode[1] do
      assign = assign .. "local" .. ASTNodeToProlog(ASTNode[1][i]) .. ", "
   end
   assign = assign:sub(1, string.len(assign) - 2) .. "], ["

   -- Add RHS expressions.
   local nRHS = #ASTNode[2]
   if nRHS > 0 then
      for i = 1, nRHS do
         assign = assign .. ASTNodeToProlog(ASTNode[2][i]) .. ", "
      end
      assign = assign:sub(1, string.len(assign) - 2)
   end

   -- Return the assignments.
   return assign .. "])"
end

-- Convert an operator node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'unop(n, e)' in the case of a unary operator or
-- 'binop(n, elhs, erhs)' in the case of a binary operator, where n is the name
-- of the operator, and e, elhs and erhs are expressions.
convert["Op"] = function(ASTNode)
   -- Get the operator name.
   local name = ASTNode[1]

   -- Get the operator's arity. Once the arity has been established, change the
   -- operator name to match the documented syntax (see documentation) if need be.
   local arity = ""
   local nOperands = #ASTNode - 1
   if (nOperands == 1) then
      arity = "unop("
   else
      arity = "binop("
   end

   -- Convert the operands.
   local operands = ""
   for i = 2, #ASTNode do
      operands = operands .. ASTNodeToProlog(ASTNode[i])
      if (i < #ASTNode) then
         operands = operands .. ", "
      end
   end
   return arity .. name .. ", " .. operands .. ")"
end

-- Convert a function call statement node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'functioncall(e, es)' where e is an expression that should
-- evaluate into a function or a table (with a metatable), and es is a list of
-- function arguments.
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

-- Convert a return statement node into Prolog.
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
-- Returns the string 'while(e, ss)' where e is an expression that evaluates into a loop
-- condition and ss is the statement block that is executed while the condition is true.
convert["While"] = function(ASTNode)
   return "while(" .. ASTNodeToProlog(ASTNode[1]) .. ", [" .. ASTNodeToProlog(ASTNode[2]) .. "])"
end

-- Convert a repeat-until loop node into Prolog.
-- param ASTNode the node to be converted.
-- Returns the string 'repeat(e, ss)' where e is an expression that evaluates into a
-- condition and ss is the statement block that is executed until the condition is met.
convert["Repeat"] = function(ASTNode)
   return "repeat(" .. ASTNodeToProlog(ASTNode[2]) .. ", [" .. ASTNodeToProlog(ASTNode[1]) .. "])"
end


-- Convert a numerical for loop node into Prolog. For loops have no special syntax and
-- instead rely on while-do statements. The expression 'for i = e1, e2, e3 do block end'
-- is equivalent to:
-- do
--    local var, limit, step = tonumber(e1), tonumber(e2), tonumber(e3)
--    if not (var and limit and step) then error() end
--    while (step > 0 and var <= limit) or (step <= 0 and var >= limit) do
--       local i = var
--       block
--       var = var + step
--    end
-- end
convert["Fornum"] = function(ASTNode)
   -- Get range values.
   local var = ASTNodeToProlog(ASTNode[2])
   local limit = ASTNodeToProlog(ASTNode[3])

   -- Get the increment value. If unspecified, then it's implicitly set to 'numbertype(1)'.
   local step = "numbertype(1)"
   local nodeLength = #ASTNode
   if (nodeLength > 4) then
      step = ASTNodeToProlog(ASTNode[4])
   end

   -- Get the instruction block.
   local block = ASTNodeToProlog(ASTNode[nodeLength])
   if #block > 0 then
      block = block .. ", "
   end

   -- Create the condition expression.
   local condition =
   "binop(or," ..
      "binop(and, binop(lt, numbertype(0), variable('step')), binop(le, variable('var'), variable('limit'))), " ..
      "binop(and, binop(le, variable('step'), numbertype(0)), binop(le, variable('limit'), variable('var')))" ..
   ")"

   -- The error function call incase an expression is not a number.
   local error = "functioncall(variable('error'), [stringtype('Expression is not a number.')])"

   return
   "do([assign([localvariable('var'), localvariable('limit'), localvariable('step')], [" ..
      "functioncall(variable('tonumber'), [" .. var .. "]), " ..
      "functioncall(variable('tonumber'), [" .. limit .. "]), " ..
      "functioncall(variable('tonumber'), [" .. step .. "]) " ..
   "]), " ..
   "if(unop(not, binop(and, variable('var'), binop(and, variable('limit'), variable('step')))), " .. error .. ", do([])), " ..
   "while(" .. condition .. ", [" ..
      "assign([localvariable('" .. ASTNode[1][1] .. "')], [variable('var')]), " ..
      block ..
      "assign([variable('var')], [binop(add, variable('var'), variable('step'))])" ..
   "])])"
end

-- Convert a generic for loop node into Prolog.
-- param ASTNode the node to be converted.
-- Just like the numeric for loop, the generic for loop relies on a while-do statement.
-- The statement 'for var_1, ..., var_n in explist do block end' is equivalent to
-- do
--    local f, s, var = explist
--    while true do
--       local var_1, ..., var_n = f(s, var)
--       var = var_1
--       if var == nil then break end
--       block
--    end
-- end
convert["Forin"] = function(ASTNode)
   -- Get the local variable list (var_1, var_2, ..., var_n).
   local variables = ""
   for i = 1, #ASTNode[1] do
      variables = variables .. "local" .. ASTNodeToProlog(ASTNode[1][i]) .. ", "
   end
   variables = variables:sub(1, string.len(variables) - 2)

   -- Get the expression list.
   local expressions = ASTNodeToProlog(ASTNode[2])

   -- Store the expression that accesses var_1 because this will be used
   -- to break the loop.
   local var_1 = ASTNodeToProlog(ASTNode[1][1])

   -- Instantiation and assignment of local variables.
   local assign = "assign([" .. variables .. "], [functioncall(variable('f'), " ..
   "[variable('s'), variable('var')])]), "

   -- Get the instruction block, if it exists.
   local nodeLength = #ASTNode
   local block = ""
   if #ASTNode[nodeLength] > 0 then
      block = ", " .. ASTNodeToProlog(ASTNode[nodeLength])
   end

   return
   "do([assign([localvariable('f'), localvariable('s'), localvariable('var')], [" .. expressions .. "]), " ..
   "while(booleantype(true), [" .. assign ..
   "assign([variable('var')], [" .. var_1 .. "]), " ..
   "if(binop(eq, variable('var'), niltype(nil)), break, do([]))" .. block .. "])])"
end

-- Convert an object-oriented call node into Prolog.
-- param ASTNode the node to be converted.
-- The colon operator is nothing more than a syntactic sugar, so this function
-- returns the string 'functioncall(e, es)', where e is an expression that evaluates
-- into a function, and es is a list of arguments, with the first item being the
-- object that invoked the function call.
convert["Invoke"] = function(ASTNode)
   local tableName = ASTNodeToProlog(ASTNode[1])
   local functionName = ASTNodeToProlog(ASTNode[2])

   -- Create the accessor. This will be used to obtain the function body.
   local access = "access(" .. tableName .. ", " .. functionName .. ")"

   -- Create the function arguments.
   local arguments = ""
   local nArguments = #ASTNode - 2
   if (nArguments > 0) then
      arguments = ", "
      for i = 3, #ASTNode do
         arguments = arguments .. ASTNodeToProlog(ASTNode[i])
         if (i < #ASTNode) then
            arguments = arguments .. ", "
         end
      end
   end
   -- Return the result, while not forgeting to add the caller argument at the
   -- front of the argument list.
   return "functioncall(" .. access .. ", [" .. tableName .. arguments .. "])"
end

-- Convert a break statement node into Prolog.
-- param ASTNode the node to convert.
-- Returns the string 'break'.
convert["Break"] = function(ASTNode)
   return "break"
end

-- Convert a local function definition node into Prolog.
-- param ASTNode the node to convert.
-- A local function definition is a syntactic sugar. The expression
-- 'local f = function() body end' translates to 'local f; f = function() body end'
convert["Localrec"] = function(ASTNode)
   local variable = "local" .. ASTNodeToProlog(ASTNode[1])
   local instantiate = "assign([" .. variable ..  "], [niltype(nil)]), "
   local assign = "assign([" .. variable ..  "], [" .. ASTNodeToProlog(ASTNode[2]) .. "])"

   -- Return the concatenation of both assign statements.
   return instantiate .. assign
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
print("chunk([" .. ASTNodeToProlog(mlp.chunk(mlp.lexer:newstream(input))) .. "]).")

-- Create command line arguments.
local nArguments = #arg
if nArguments > 1 then
   local arguments = ""
   for i = 2, nArguments do
      arguments = arguments .. toStringType(arg[i]) .. ", "
   end
   print("arguments([" .. arguments:sub(1, string.len(arguments) - 2) .. "]).")
else
   print("arguments(niltype(nil)).")
end
