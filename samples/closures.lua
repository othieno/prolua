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

-- A simple function to return the square of a value.
local function square(x)
   return x * x
end

-- A table where function objects will be stored.
local math = {}

-- A function to calculate the factorial of a given value.
math.factorial = function(n)
   if n > 0 then
      return n * math.factorial(n - 1)
   else
      return 1
   end
end


-- A naive implementation to calculate the fibonacci number.
-- Warning, Prolua takes time calculating values of n greater than 13.
-- This is a good function to test Prolua's inefficiency (pay attention to
-- the number of execution context graph nodes).
math["fibonacci"] = function(n)
   if n < 2 then
      return n
   else
      return math["fibonacci"](n - 1) + math["fibonacci"](n - 2)
   end
end


-- A simple function to test closures.
-- Taken from http://en.wikipedia.org/wiki/Lua_%28programming_language%29#Functions
function addto(x)
   return function(y)
      return x + y
   end
end
fourplus = addto(4)


return square(26), math.factorial(256), math.fibonacci(8), fourplus(3)
