factorial = lambda x: x and x * factorial(x - 1) or 1
power = lambda x,y: y and x * power(x, y - 1) or 1

a = lambda k,n: factorial(n) / factorial(n - k)
a_ = lambda k, n: power(n, k)
p = lambda n: factorial(n)
c = lambda k,n: factorial(n) / (factorial(k) * factorial(n - k))
c_ = lambda k,n: factorial(n + k - 1) / (factorial(k) * factorial(n - 1))
