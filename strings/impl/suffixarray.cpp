#include <assert.h>

#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>  // for gcd
#include <queue>
#include <set>
#include <stack>
#include <vector>
#include <bitset> // instead of bool[N];
#include <stdlib.h> // for exit()

// https://codeforces.com/blog/entry/11080
#include <ext/pb_ds/assoc_container.hpp>  // Common file
#include <ext/pb_ds/detail/standard_policies.hpp>
#include <ext/pb_ds/tree_policy.hpp>  // Including tree_order_statistics_node_update

using ll = long long;
using namespace std;



struct sorter {
    pair<int, int> rank;
    int ix;

    bool operator <(const sorter &other) {
        return rank < other.rank;
    }
};


// returns suffix array.
// can I increment by *2+1?
vector<int> rankedSuffixes(string s) {
    const int n = s.size();
    assert(s[n-1] == '$');

    // index of string suffix space -> rank.
    vector<int> str2rank(n);
    // rank space -> index of string suffix.
    bool cur = 0;
    int len = 1;
    
    vector<sorter> rank2str[2];
    rank2str[0].resize(n); rank2str[1].resize(n);
    for(int i = 0; i < n; ++i) {
        rank2str[cur][i].rank.first = rank2str[cur][i].rank.second = s[i] - 'a';
        rank2str[cur][i].ix = i;
    }
    
    sort(rank2str[cur].begin(), rank2str[cur].end());
    for(int i = 0; i < n; ++i) {
        str2rank[rank2str[cur][i].ix] = i;
    }

    while(len <= n) {
        const bool next = cur ^ 1;
        for(int i = 0; i < n; ++i) {
            rank2str[next][i].ix = i;
            rank2str[next][i].rank.first = str2rank[i];
            rank2str[next][i].rank.second = i + len >= n ? str2rank[i] : str2rank[i + len];
        }
        cur = next;
        len *= 2;
        sort(rank2str[cur].begin(), rank2str[cur].end());
    
        for(int i = 0; i < n; ++i) {
            str2rank[rank2str[cur][i].ix] = i;
        }
    }
    vector<int> out(n);
    for(int i = 0; i < n; ++i) { out[i] = rank2str[cur][i].ix; }
    return out;
}

// return array such that kasai[i] = lcp(suffix[i-1], suffix[i])
vector<int> kasai(const vector<int> &rank2str, const vector<int> &str2rank, const string &s) {
  vector<int> lcps(s.size() + 1, 0);

  int lcplowerbound = 0;
  for(int si = 0; si < s.size(); ++si) {
    // prefix index
    int pi = str2rank[si];
    if (pi == 0) { continue; }
    int ti = rank2str[pi-1];
    while(s[si + lcps[pi]] == s[ti + lcps[pi]]) { lcps[pi]++; }
    lcplowerbound = lcps[pi]-1;
  }
};


int main() {
    string s; cin >> s;
    s += '$';

    const int n = s.size();
    vector<int> rank2str = rankedSuffixes(s);

    cout << "sorted suffixes:\n";
    for(int i = 0; i < n; ++i) {
        cout << s.substr(rank2str[i]) << "\n";
    }

    std::ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    return 0;
}