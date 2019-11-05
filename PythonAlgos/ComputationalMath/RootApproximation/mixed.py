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

    curra = a
    currb = b
    print(f"A={curra}; B={currb} => X={(curra + currb) / 2}; F(X)={f((curra + currb) / 2)}")
 
    shouldStop = False
    while True:
        if f(curra) * ddf(curra) < 0:
            curra = curra - f(curra) * (curra - currb) / (f(curra) - f(currb))

        elif f(curra) * ddf(curra) > 0:
            curra = curra - f(curra) / df(curra)

        if f(currb) * ddf(currb) < 0:
            currb = currb - f(currb) * (currb - curra) / (f(currb) - f(curra))

        elif f(currb) * ddf(currb) > 0:
            currb = currb - f(currb) / df(currb)

        print(f"A={curra}; B={currb} => X={(curra + currb) / 2}; F(X)={f((curra + currb) / 2)}")
        if abs(curra - currb) <= 2 * eps:
            if shouldStop:
                break

            shouldStop = True

