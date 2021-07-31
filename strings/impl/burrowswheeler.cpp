#include <algorithm>
#include <assert.h>
#include <iomanip>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>
using namespace std;

const char TERMINAL = '$';
string encode(string s) {
  s += TERMINAL;

  cout << "encode(" << s << "):\n";

  const int n = s.size();
  vector<string> t(n);
  for (int d = 0; d < n; ++d) {
    t[d].resize(n);
    for (int i = 0; i < n; ++i) {
      t[d][i] = s[(i + d) % n];
    }
  }
  sort(t.begin(), t.end());

  for(int i = 0; i < n; ++i) {
    cout << "  [" << i << "]" << t[i] << "\n";
  }
  
  string out;
  out.resize(n);

  for (int i = 0; i < n; ++i) {
    out[i] = t[i][n - 1];
  }
  return out;
}

string decode(const string &l) {
  map<char, int> k; // char -> num of occurrence.
  vector<int> c(l.size(), 0); // index -> # of occurrence of l[c] in [0, index) 

  for(int i = 0; i < l.size(); ++i) {
    c[i] = k[l[i]]; 
    k[l[i]]++;
  }

  // m: char -> location of 1st occurrence of char c in sorted order.
  map<char, int> m;
  int tot = 0;
  for(auto it: k) {
    m[it.first] = tot;
    tot += it.second;
  }

  string out; out.resize(l.size());
  int ix = 0;
  for(ix = 0; l[ix] != TERMINAL; ++ix) {}

  for(int i = l.size() - 1; i >= 0; i--) {
    out[i] = l[ix];
    ix = c[ix] + m[l[ix]];
  }
  return out;
}

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
    std::string enc = encode(s);
    std::string dec = decode(enc);

    std::cout << "s: " << setw(NCOLS) << s << " | enc: " << setw(NCOLS) << enc
              << " | dec: " << setw(NCOLS) << dec << "\n";
    assert(s + TERMINAL == dec);
  }

  return 0;
}
