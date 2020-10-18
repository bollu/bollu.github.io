#include <iostream>
#include <climits>
using namespace std;

bool x_gt_x_plus_1(unsigned x) {
    return (x + 1) > x;
}

int main() {
    cout << x_gt_x_plus_1(0) << "\n";
    cout << x_gt_x_plus_1(UINT32_MAX) << "\n";
}
