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

mt_a =
{
   __concat = function(self, value)
      if type(value) == "string" then
         return "Hello," .. value
      end
   end
}
t_a = setmetatable({"H", "e", "l", "l", "o", ","}, mt_a)
a = t_a .. " World!" -- "Hello, World!"


mt_b =
{
   __add = function(lhs, rhs)
      return {value = lhs.value + rhs.value}
   end
}
t_b = setmetatable({value = 5}, mt_b)
b = (t_b + t_b).value -- 10


mt_c =
{
   __tostring = function(t)
      return t["stringValue"]
   end
}
t_c = setmetatable({stringValue = "The string to print."}, mt_c)
c = tostring(t_c) -- "The string to print."


mt_d =
{
   __call = function(t, a, b, c, d)
      return t[1] * (a + b + c + d)
   end
}
t_d = setmetatable({100}, mt_d)
d = t_d(1, 2, 3, 4) -- 1000


return a, b, c, d
