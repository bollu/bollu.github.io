#include <assert.h>
#include <iomanip>
#include <iostream>
#include <stack>
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
vector<int> mkz(const string &s) {
  const int n = s.size();
  int tick = 0;
  vector<int> len(n, 0);
  int l = 0; // overhang location
  for (int i = 1; i < n; ++i) {
    tick++;
    // use overhang to get estimate.
    if (i < l + len[l]) {
      // can't be larger than the overhang itself!
      len[i] = max<int>(0, min<int>(l + len[l] - i, len[i - l]));
    }

    while (i + len[i] < n && s[len[i]] == s[i + len[i]]) {
      tick++;
      len[i]++;
    }

    if (i + len[i] >= l + len[l]) {
      l = i;
    }
  }
  if (tick >= 3 * n) {
    std::cout << "**ERROR: expected tick:" << tick << "  to be < 3.(n:" << n
              << "):" << 3 * n << ". \n";
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

vector<int> mkwitness(const std::string &s) {
  const int n = s.size();
  vector<int> out(n);
  vector<int> z = mkz(s);
  out[0] = -1;
  for (int i = 1; i < n; ++i) {
    int maxlen = n - i;
    if (z[i] == maxlen) {
      out[i] = -1;
    } else {
      out[i] = z[i];
    }
  }
  return out;
}

void check_witness(const std::string &s, const vector<int> &ws) {
  assert(s.size() == ws.size() && "expected witness for each string");
  const int n = s.size();
  for (int i = 0; i < n; ++i) {
    cout << "[" << i << "]" << s[i] << " ";
  }
  cout << " | witness: ";
  for (int i = 0; i < n; ++i) {
    cout << "[" << i << "]" << ws[i] << " ";
  }
  cout << "\n";

  for (int i = 0; i < n; ++i) {
    if (ws[i] == -1) {
      for (int len = 0; i + len < n; ++len) {
        if (s[len] != s[i + len]) {
          cout << "expected s[" << len << "]:" << s[len] << " = "
               << "s[i:" << i << "+len:" << len << "=i+len"
               << "]:" << s[i + len] << "\n";
        }
        assert(s[len] == s[i + len]);
      }
    } else {
      if (i + ws[i] >= n) {
        cout << "expected ws[i:" << i << "]:" << ws[i]
             << " to satisfy (i + ws[i]):" << i + ws[i] << " < n:" << n << "\n";
      }
      assert(i + ws[i] < n);
      if (s[ws[i]] == s[i + ws[i]]) {
        cout << "expected ws[i:" << i << "]:" << ws[i]
             << " to distinguish between s[ws[i]:" << ws[i] << "]:" << s[ws[i]]
             << " and s[i+ws[i]:" << i + ws[i] << "]:" << s[i + ws[i]] << "\n";
      }
      assert(s[ws[i]] != s[i + ws[i]]);
    }
  }
}

bool consistent(const vector<int> &ws, int l, int r) {
  assert(l < r);
  if (l + ws.size() < r) {
    return true;
  }
  // intuition???

  //             r#####
  //       l************
  // ------------------
  //
  const int k = ws[r - l];
  if (k == -1) {
    return true;
  }
  return false;
}

enum class duelresult { removel, remover };

duelresult duel(const string &needle, const string &haystack,
                const vector<int> &needleWitnesses, int l, int r) {
  // we have alignment between haystack@0, needle@l, needle@r
  assert(!consistent(needleWitnesses, l, r));
  const int k = needleWitnesses[r - l];
  if (haystack[r + k] == needleWitnesses[k]) {
    return duelresult::removel;
  } else {
    return duelresult::remover;
  }
}

bool substring(const string &needle, const string &haystack) {
  const vector<int> needleWitnesses = mkwitness(needle);
  vector<int> is; // consistent indexes
  const int n = haystack.size();
  for (int i = n - 1; i >= 0; i--) {
    // is[0] > is[1] > ...
    is.push_back(i);

    while (is.size() > 2) {
      int r = is[is.size() - 1];
      int l = is[is.size() - 2];
      if (consistent(needleWitnesses, l, r)) {
        break;
      }
      if (duel(needle, haystack, needleWitnesses, l, r) ==
          duelresult::removel) {
        is.push_back(r);
      } else {
        is.push_back(l);
      }

    } // end while loop of conflict.
  }
  vector<bool> text1(haystack.size(), false);
  int k = -1;
  int nextis = 0;
  for (int i = 0; i < n; ++i) {
    if (nextis < is.size() && i == is[nextis]) {
      k = i;
    }
    if (k == -1 || needle[i - k + 1] != haystack[i]) {
      text1[i] = 0;
    } else {
      text1[i] = 1;
    }
  }

  int l = 0;
  // [l, r]
  for (int r = 0; r < n; ++r) {
    if (text1[r] == 0) {
      l = r;
      continue;
    }
    assert(text1[r] == 1);
    if (text1[l] == 0) { l = r; }
    int len = r - l + 1;
    if (len == needle.size()) {
      return l; // match at left.
    }
  }
  // no match.
  return -1;
}


int main() {
  const int K = 3;
  const int NDIGITS = 6;
  const char *substr = "abc";
  for (int i = pow(K, NDIGITS); i < pow(K, NDIGITS + 2); ++i) {
    std::string s = basek(i, K);
    vector<int> witness = mkwitness(s);
    check_witness(s, witness);
  }
  return 0;
}
