#include <iomanip>
#include <iostream>
#include <algorithm>
#include <map>
#include <queue>
#include <set>
#include <string>
#include <vector>
#include <assert.h>
using namespace std;
using ll = long long;
using rr = double;

// Kasai's video: https://www.youtube.com/watch?v=VpEumOuakOU
// key idea: walk suffixes in TEXT ORDER, not SUFFIX ARRAY ORDER!

// lcp(s, t) = longest common prefix
// LCP(s, t) = |lcp(s, t)| = len. of longest common prefix.
// LCP array:
// let A[i] be the suffix array.
// lcp[i] = LCP(A[i], A[i+1])

// key lemma:
// LCP(A[l], A[r]) = min{l <= k < r : LCP(A[k], A[k+1])}

// For all k between l and r - 1: LCP(A[k], A[k + 1]) ≥ m.
// So there is a prefix of length m common for all suffixes A[l], A[l + 1],… ,A[r]. 
// So LCP(A[l], A[r]) ≥ m. 
//     However, if LCP(A[k],A[k+1]) = m for some k between l and (r-1),
//     then character number (m + 1) has changed between k-th and k+1-th suffix and so
//     it cannot be the same in A[l] and A[r], so LCP(A[l],A[r]) ≤ m.
//     Thus, LCP(A[l], A[r]) = m.

// How to compute LCP array?
// 1. startby computing LCP(A[0], A[1]).
// 2. Instead of computing LCP(A[1], A[2]), move A[0] and A[1]
//    one position to the right to get new suffixes A[i] and A[j] (we don't know where these will be in the sorted suffixes list). 

// eg: s = ababdabc$
// Let A[0], A[1], A[2] ... be the suffixes of s in lex order.
// so S[A[0]:] < S[A[1]:] < S[A[2]:] ..  < S[A[n]:]
// A[0] = 8:$
// A[1] = 0:ababdabc$
// A[2] = 5:abc$
// A[3] = 2:abdabc$
// A[4] = 1:babdabc$
// A[5] = 6:bc$
// A[6] = 3:bdabc$
// A[7] = 7:c$
// A[8] = 4:dabc$
// drop: maps a suffix ordinal `i` to the suffix ordinal of `compress[decompress[i]+1]`.

// lcp[i] = LCP(i-1, i)
// now lcp[0] = LCP(A[-1], A[0]) := 0  [by defn]  L
// lcp[1] = LCP(A[0], A[1]) = ($, ababdabc$) = |""| = 0
// now lcp[1] = LCP(A[], A[2]) = LCP(ababdabc$, abc$) = |"ab"| = 2
// notice that LCP(A[drop[1]], A[drop[2]]) = LCP(babdabc$, bc$) = LCP(4, 5) |"b"| = 1

vector<pair<string, int>> suffixes_sorted_slow(string s) {
  assert(s[s.size() - 1] == '$');
  const int n = s.size();
  vector<pair<string, int>> sufs;
  for(int i = 0; i < n; ++i) {
    sufs.push_back({string(s, i), i});
  }

  std::sort(sufs.begin(), sufs.end());
  return sufs;
}

int main() {
  string s = "ababdabc$";
  // decompress: given an ordinal, return the corresponding text index of the suffix.
  vector<pair<string, int>> decompress = suffixes_sorted_slow(s);
  for(int i = 0; i < s.size(); ++i) {
    cout << "A[" << i << "] = " << decompress[i].second << ":" << decompress[i].first << "\n";
  }

  // compress: given a text index of a suffix, return the compressed ordinal.
  vector<int> compress(s.size());
  for(int i = 0; i < s.size(); ++i) {
    compress[decompress[i].second] = i;
  }

  // === LCP===
  int lcp = 0;
  int ticks = 0; // clock ticks for complexity
  vector<int> lcps(s.size());

  // i: text position.
  // Core idea: walk through text in text position. Notice that we can
  // remember the LCP: 
  // lcp(s[i+1:], prev_suffix s[i+1:]) >= lcp(drop s[i:], drop prev_suffix s[i:]) = lcp(s[i:], prev_suffix s[i:])-1
  // why? because when we chop the first index of the string, we will get some relation like this:
  // 
  //  TABLE OF ORDERED SUFFIXES
  //  --------------------------
  //  1. drop (prev_suffix s[i:])-------+           
  //                                    |lcp(i)-1
  //                                    |
  //   ...                              |
  //   ...                              |
  //   ...                              |
  // 4. prev_suffix s[i+1:]-------------|-------+
  //                                    |       | >= lcp(i)-1
  // 5. s[i+1:] (=drop s[i:]) ----------+-------+
  //
  //
  // 8. prev_suffix s[i:] ---------+
  //                               | lcp(i)
  // 9. s[i:] ---------------------+
  for(int i = 0; i < s.size(); ++i) {
    ticks++;
    // drop a character from previous match.
    int lcp = max<int>(0, lcp-1);
    if (compress[i] == 0) { 
      lcps[compress[i]] = -1; continue;
    }

    assert(compress[i] > 0 && 
            "string is somehow smallest in lex order "
            "while not being $");
    //  string prior to this one in suffix order.
    const int j = decompress[compress[i]-1].second;
    while(i + lcp < s.size() &&
          j + lcp < s.size() &&
          s[i+lcp] == s[j + lcp])  {
      lcp++;
      ticks++;
    }
    lcps[compress[i]] = lcp;
  }

  // linear time complexity.
  assert(ticks < s.size() * 2);

  // verify LCP
  assert(lcps[0] == -1);
  for(int i = 1; i < s.size(); ++i) {
    int ix1 = decompress[i].second, ix2 = decompress[i-1].second;
    int len = 0;
    while(ix1 + len < s.size() && ix2 + len < s.size() && s[ix1+len] == s[ix2+len]) {len++;}
    cout << "lcps[" << i << "] compares:\n";
    cout << "\t- " << decompress[i].first << "\n";
    cout << "\t- " << decompress[i-1].first << "\n";
    cout << "\t- lcp(kasai): " << lcps[i] << "\n";
    cout << "\t- lcp(brute): " << len << "\n";
    assert(lcps[i] == len);
  }

  return 0;
}
