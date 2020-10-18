#include <iostream>
#include <climits>
using namespace std;

bool f(unsigned x) {
    return (x + 1) > x;
}

int main() {
    cout << "f(0): " << f(0) << "\n";
    cout << "f(UINT32_MAX):" << f(UINT32_MAX) << "\n";
    cout << "UNIT32_MAX: " << UINT32_MAX << "\n";
    cout << "1 + UINT32_MAX: " << 1 + UINT32_MAX << "\n";
}
