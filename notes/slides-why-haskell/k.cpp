#include <iostream>
int k(int x, int y) { return x; }
int err() { exit(1); return 0; }
int main() {
    std::cout << k(10, 20)  << " " << 10 << "\n";
    std::cout << k(10, err())  << " " << 10 << "\n";
}
