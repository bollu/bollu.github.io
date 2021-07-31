#include <assert.h>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

const int COLLEN = 10;

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

  // reverse string.
  for (int i = 0; i < n / 2; ++i) {
    std::swap(s[i], s[n - 1 - i]);
  }
  return s;
}

// == STRING ALGORITHM ==
vector<int> mkz(string s) {
  const int n = s.size();
  int tick = 0;
  vector<int> len(n, 0);
  int l = 0; // overhang location
  for (int i = 1; i < n; ++i) {
    tick++;
    // use overhang to get estimate.
    if (i < l + len[l]) {
      // can't be larger than the overhang itself!
      len[i] = max<int>(0, min<int>(l + len[l] - i, len[i-l]));
    }
    
    while (i + len[i] < n && s[len[i]] == s[i + len[i]]) {
      tick++;
      len[i]++;
    }
    
    if (i + len[i] >= l + len[l]) {
      l = i;
    }
  }
  if (tick >= 3*n) {
    std::cout << "**ERROR: expected tick:" << tick
	      << "  to be < 3.(n:" << n << "):" << 3*n << ". \n";
  }
  
  assert(tick <= 3 * n && "Z must be linear!");
  return len;
}

void check_z(const std::string &s, const vector<int> &z) {
  assert(z.size() == s.size());
  cout << "pfx " << s << " ";
  for (int i = 0; i < s.size(); ++i) {
    cout << "[" << i << "]" << s[i] << ";" << z[i] << " ";
  }
  cout << "\n";

  for (int i = 0; i < s.size(); ++i) {
    cout << "checking prefix for string |" << s << "| at index |" << i
         << "|.\n";

    int len = 0;
    // z[0] is defined as 0, because z is only defined for proper suffixes.
    if (i > 0) {
      while (i + len < s.size() && s[len] == s[i + len]) {
        len++;
      }
    }

    if (z[i] != len) {
      cout << "incorrect z-fn value for string |" << s << "| "
           << "at index |" << i << "|. Expected |" << len << "| "
           << "found |" << z[i] << "|\n";
    }
    assert(z[i] == len);
  }
}

int main() {
  const int K = 3;
  const int NDIGITS = 6;
  const char *substr = "abc";
  for (int i = pow(K, NDIGITS); i < pow(K, NDIGITS + 2); ++i) {
    std::string s = basek(i, K);
    vector<int> z = mkz(s);
    check_z(s, z);
  }
  return 0;
}
