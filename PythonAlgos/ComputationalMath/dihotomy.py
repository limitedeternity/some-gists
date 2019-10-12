def find(f, a, b, eps):
    start = a
    end = b

    if not (f(a) * f(b) < 0):
        return

    while True:
        done = False
        if abs(start - end) < eps:
            done = True

        c = (start + end) / 2
        print(f"[{start}; {end}]: X={c} -> F(x)={f(c)}")

        start = start if f(a) * f(c) < 0 else c
        end = c if f(a) * f(c) < 0 else end

        if done:
            break


find(lambda x: x**4 - x - 1, 1.1, 1.3, 0.001)
