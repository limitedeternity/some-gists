def swap(L, K):
    return [L[:a]+[L[b]]*(B-b+1)+L[A+1:b]+[L[a]]*(A-a+1)+L[B+1:] for (a, A), (b, B) in [(lambda g: [g[2*k:2*k+2] for k in sorted(K)])([i for i, x in enumerate(L) if L[max(0,i-1):i+2].count(x) == 2])]][0]

# swap([2, 2, 4, 8, 8, 6, 1, 5, 5, 5, 2, 8, 6, 3, 4], (1, 2)) --> [2, 2, 4, 5, 5, 5, 6, 1, 8, 8, 2, 8, 6, 3, 4]