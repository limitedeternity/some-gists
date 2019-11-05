def find(f, df, ddf, a, b, eps):
    fa = f(a)
    dfa = df(a)
    ddfa = ddf(a)
    fb = f(b)
    dfb = df(b)
    ddfb = ddf(b)

    if not (
        (fa * fb < 0) and
        ((dfa > 0 and dfb > 0) or (dfa < 0 and dfb < 0)) and
        ((ddfa > 0 and ddfb > 0) or (ddfa < 0 and ddfb < 0))
    ):
        return

    currx = a if fa * ddfa > 0 else b
    print(f"X={currx}; F(X)={f(currx)}")

    shouldStop = False
    while True:
        prevx = currx
        currx = prevx - f(prevx)**2 / (f(prevx + f(prevx)) - f(prevx))

        print(f"X={currx}; F(X)={f(currx)}")
        if abs(currx - prevx) < 2 * eps:
            if shouldStop:
                break

            shouldStop = True

