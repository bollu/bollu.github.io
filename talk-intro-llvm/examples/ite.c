int g;

int foo() {
    int z;
    int x = g + 1;
    if (x < 10) {
        z = 20;
    } else {
        z = 30;
    }
    return z + 10;
}

