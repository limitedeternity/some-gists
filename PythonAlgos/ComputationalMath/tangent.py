def find(f, df, ddf, x0, x1, eps):
    fx0 = f(x0)
    dfx0 = df(x0)
    ddfx0 = ddf(x0)
    fx1 = f(x1)
    dfx1 = df(x1)
    ddfx1 = ddf(x1)

    if not (
        (fx0 * fx1 < 0) and
        ((dfx0 > 0 and dfx1 > 0) or (dfx0 < 0 and dfx1 < 0)) and
        ((ddfx0 > 0 and ddfx1 > 0) or (ddfx0 < 0 and ddfx1 < 0))
    ):
        return

    currx = x0 if fx0 * ddfx0 > 0 else x1
    print(f"X={currx}; F(X)={f(currx)}; DF(X)={df(currx)}")

    while True:
        prevx = currx
        currx = prevx - f(prevx) / df(prevx)

        print(f"X={currx}; F(X)={f(currx)}; DF(X)={df(currx)}")
        if abs(currx - prevx) < eps:
            print(
                f"X={currx - f(currx) / df(currx)}; F(X)={f(currx - f(currx) / df(currx))}; DF(X)={df(currx - f(currx) / df(currx))}")
            break
