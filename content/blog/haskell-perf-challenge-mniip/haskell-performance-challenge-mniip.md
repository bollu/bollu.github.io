+++
title = "Haskell-Performance-Challenge-Mniip"
date = "2018-10-08T01:41:23+05:30"
draft = true

+++
`mniip` recently challenged me to get the same performance
as this piece of code:

```c
#include <stdio.h>

char sieve[100000000 >> 3];

int main()
{
    int count = 0;
    for(long int i = 2; i < 100000000; i++)
        if(!(sieve[i >> 3] & (1 << (i & 7))))
        {
            count++;
            for(long int j = i * i; j < 100000000; j += i)
                sieve[j >> 3] |= 1 << (j & 7);
        }
    printf("%ld\n", count);
}
```

A wise man once said, "to profile a haskell application, one must rebuild the universe."