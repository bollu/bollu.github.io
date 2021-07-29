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

string decode(string rightmost) {
  // given last column, we now first column by sorting characters of s.
  // map nth occurrence of character to its index.

  string css[2];
  css[0] = css[1] = rightmost;
  sort(css[0].begin(), css[0].end());

  // c=0                 c=1
  // ----------------------
  // $0                  .
  // a0                  .
  // a1                  .
  // a2                  .
  // b0                  .
  // n0                  .
  // n1                  .

  const int n = rightmost.size();

  map<char, int> occs[2];
  map<pair<char, int>, int> char2row[2];
  map<int, pair<char, int>> row2char[2];

  cout << "decode(" << rightmost << "):\n";
  for (int lr = 0; lr < 2; ++lr) {
    for (int r = 0; r < n; ++r) {
      char c = css[lr][r];
      int count = occs[lr][c];
      char2row[lr][std::make_pair(c, count)] = r;
      row2char[lr][r] = std::make_pair(c, count);
      occs[lr][c]++;
    }


    cout << "  lr[" << lr << "] = ";
    for(int i = 0; i < n; ++i) {
      cout << "[" << i << "]" << row2char[lr][i].first << row2char[lr][i].second << " ";
      // check that these are inverses.
      assert(char2row[lr][row2char[lr][i]] == i);
    }
    cout << "\n";
  }

  

  // we are at the right column, at the $0.
  // TODO: what happens if I start at other column?
  int row = char2row[0][{TERMINAL, 0}];

  string t;
  while (t.size() != rightmost.size()) {
    t += row2char[0][row].first;
    t += row2char[1][row].first;
    row = char2row[0][row2char[1][row]];
  }

  reverse(t.begin(), t.end());
  return t;
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
