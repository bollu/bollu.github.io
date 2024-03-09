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

vector<int> prefixfn(const std::string &s) {
  int blen = 0;
  vector<int> out(s.size(), 0);
  const int n = s.size();

  // border[0] = 0.
  for (int i = 1; i < n; i++) {
    if (s[i] == s[blen]) {
      blen++;
      out[i] = blen;
      // string "aa" can have blen 1 at i 1.
      assert(out[i] <= i);
    } else {
      while (blen > 0) {
        blen--;
        blen = out[blen];
        if (s[i] == s[blen]) {
          blen++;
          out[i] = blen;
          assert(out[i] <= i);
          break;
        }
      }

      if (blen == 0 && s[i] == s[blen]) {
        blen++;
        out[i] = blen;
          assert(out[i] <= i);
      }
    }
  }
  return out;
}

void check_prefix(const std::string &s, const vector<int> &pfx) {
  assert(pfx.size() == s.size());
  cout << "pfx " << s << " ";
  for(int i = 0; i < s.size(); ++i) {
    cout << "[" << i << "]" << s[i] << ";" << pfx[i] << " ";
  }
  cout << "\n";

  for (int i = 0; i < s.size(); ++i) {
    cout << "checking prefix for string |" << s << "| at index |" << i
         << "|.\n";
    int bestlen = 0;
    // check if len is possible.
    for(int len = 1; len <= i; ++len) {
      bool fit = true;
      // s[0, len) = s(i-len, i]
      for(int k = 0; k < len; ++k) {
        fit &= s[k] == s[i-len+1+k];
        // cout << "\t |i " << i << " |len " << len << " |k " << k << " |s[k] " << s[k] << " | s[i-len+1+k]" << s[i-len+1+k] << " |fit " << fit << "\n";
      }
      if (fit) { bestlen = len; }
    }

    if (pfx[i] != bestlen) {
      cout << "incorrect prefix fn value for string |" << s << "| "
           << "at index |" << i << "|. Expected |" << bestlen << "| "
           << "found |" << pfx[i] << "|\n";
    }
    assert(pfx[i] == bestlen);
  }
}

// needle, haystack
int kmp(const std::string &needle, const std::string &haystack) {
  std::string search;
  search += needle;
  search += "$";
  search += haystack;

  const vector<int> borders = prefixfn(search);
  check_prefix(search, borders);
  for (int i = needle.size(); i < search.size(); ++i) {
    if (borders[i] == needle.size()) {
      // matched at i. This means that
      // s[0:needlesz) = s(i-needlesz:i]
      // so string starts at (i-needlesz+1).
      // but we have prepended "needle + $" to the string, so we need to subtract (needlesz+1).
      // this brings us to (i - needlesz + 1) - (needlesz + 1) = i - 2needlesz
      return i - 2 * needle.size();
    }
  }
  return -1;
}

int main() {
  const int K = 3;
  const int NDIGITS = 6;
  const char *substr = "abc";
  for (int i = pow(K, NDIGITS); i < pow(K, NDIGITS + 2); ++i) {
    std::string s = basek(i, K);
    int ix = s.find(substr);
    int ix2 = kmp(substr, s);
    if (ix != ix2) {
      std::cout << "mismatch at string |" << s << "| for substr |" << substr
                << "|."
                << "expected substring at |" << ix << "| but found at |" << ix2
                << "|\n";
    }
    std::cout << "str " << std::setw(COLLEN) << s << " | substr "
              << std::setw(COLLEN) << substr << " | ix "
              << std::setw(COLLEN) << ix << " | ix2"
              << std::setw(COLLEN) << ix2 << (ix != -1 ? " ###### " : "") << "\n";
    assert(ix == ix2);
  }
  return 0;
}
