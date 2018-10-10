#include <stdio.h>

static const int SIZE = 100;
char sieve[SIZE >> 3];

int main()
{
    int count = 0;
    for(long int i = 2; i < SIZE; i++)
        if(!(sieve[i >> 3] & (1 << (i & 7))))
        {
            count++;
            for(long int j = i * i; j < SIZE; j += i)
                sieve[j >> 3] |= 1 << (j & 7);
        }
    printf("%ld\n", count);
}
