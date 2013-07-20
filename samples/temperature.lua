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

-- This code converts the temperatures 5^n, for n in [1, 5] from fahrenheit to celsius.

function toCelsius(fahrenheit)
	return (fahrenheit - 32)*(5 / 9);
end;

-- Calculate 5 different temperatures.
t = {min = 0, 0, 0, 0, max = 0}

t.min = toCelsius(5);
local i = 1;
while (i < 4) do
	t[i] = toCelsius(5^(i + 1));
	i = i + 1;
end;
t.max = toCelsius(5^5);

-- Return the calculated temperatures.
return t.min, t[1], t[2], t[3], t.max;
