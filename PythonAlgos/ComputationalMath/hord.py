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

    mval = dfx0 if dfx0 < dfx1 else dfx1

    prevx = x0
    currx = x1

    print(f"X={prevx}; F(X)={fx0}\nX={currx}; F(X)={fx1}")

    while True:
        x = currx - (currx - prevx) * f(currx)/(f(currx) - f(prevx))
        print(f"X={x}; F(X)={f(x)}")

        if abs(f(x)) / mval <= eps:
            print(
                f"X={x - (x - currx) * f(x)/(f(x) - f(currx))}; F(X)={f(x - (x - currx) * f(x)/(f(x) - f(currx)))}")
            break

        prevx = currx
        currx = x
