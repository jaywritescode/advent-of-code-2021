Notes for problem 3
===================

`n & (1 << k) == 0` iff the $k$-th bit of $n$ is zero.

For $n < 2^k$, `n ^ ((1 << k) - 1)` will flip all of the bits of $n$.