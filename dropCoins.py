from sympy import binomial

def prob(nDrops, nCoins):
    """Compute the probability of drop [nCoins] coins for [nDrops] times, 
    showing heads for all the [nDrops] times for at least one coin.

    >>> prob(10, 1000) = ~0.63
    """
    a = 2**nDrops
    y = 0
    for i in range(1, nCoins+1):
        y += binomial(nCoins, i) / (a - 1)**i
    return y * ((a-1)/(a))**nCoins

print(prob(10,1000))
