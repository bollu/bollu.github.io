int foo(int x) {
    int p = 2;
    for(int i = 0; i < 10; i += 2) {
        p *= 10 * x;
    }
    return p;
}
