def fib(n)
  a = 1
  b = 0
  while n != 0
    a, b = a + b, a
    n -= 1
  end
  b
end

puts fib(8)
