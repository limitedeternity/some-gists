def find(f, df, startx, endx, eps):
    fstartx = f(startx)
    dfstartx = df(startx)
    dendx = f(endx)
    dfendx = df(endx)

    maxval = dfstartx if dfstartx > dfendx else dfendx
    minval = dfstartx if dfstartx < dfendx else dfendx
    l = 1 / maxval
    q = 1 - minval / maxval
    print(f"l={l}; q={q}\n")

    currx = startx
    print(f"X={currx}; F(X)={fstartx}")

    while True:
        x = currx - l * f(currx)
        print(f"X={x}; F(X)={f(x)}")

        if (abs(x - currx) < eps):
            print(f"X={x - l * f(x)}; F(X)={f(x - l * f(x))}")
            break

        currx = x
