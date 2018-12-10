def binomialCoef(n, r):
    """Calculate binomial coefficient 
           nCr = (n*(n-1)*...*(n-r+1))/(1*2*...*r),
       which is equal to sympy.binomial(n,r), but sometimes faster
       
    >>> binomialCoef(9,2) = 36
    """
    assert n>=0
    assert 0<=r<=n
    if (r > n-r):
        return nCr(n,n-r)
    # n/1*(n-1)/2*...*(n-r+1)/r
    c = 1 
    for i in range(1,r+1):
        c = (c*(n-i+1)) // i
    return c
