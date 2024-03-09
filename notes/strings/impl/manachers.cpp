#include <algorithm>
#include <assert.h>
#include <iomanip>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>
using namespace std;

// === problem generation stuff ===

int pow(int n, int i) {
  if (i == 0) {
    return 1;
  }
  if (i == 1) {
    return n;
  }
  int p = pow(n, i / 2);
  if (i % 2 == 0) {
    return p * p;
  } else {
    return p * p * n;
  }
}

std::string basek(int n, int k) {
  if (n == 0) {
    return "a";
  }
  std::string s;
  while (n != 0) {
    s.push_back('a' + (n % k));
    n /= k;
  }
  return s;
}

int main() {
  const int K = 3;
  const int NDIGITS = 6;
  const int NCOLS = 10;
  for (int i = pow(K, NDIGITS); i < pow(K, NDIGITS + 2); ++i) {
    std::string s = basek(i, K);
  }

  return 0;
}
