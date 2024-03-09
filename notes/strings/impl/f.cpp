#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <vector>
#include <iomanip>
using namespace std;
using ll = long long;
using rr = double;


// = E[# of turns to eat all sushi]
// 
static const int N = 3001;
int dp [N][N];
int main() {
  std::string s, t;
  cin >> s >> t;

  dp[0][0] = 0; // length of longest common is 0.
  for(int lens = 0; lens <= s.size(); ++lens) {
    dp[lens][0] = 0; // longest common with string of length 0 is 0.
  }

  for(int lent = 0; lent <= s.size(); ++lent) {
    dp[0][lent] = 0; // longest common with string of length 0 is 0.
  }

  for(int lens = 1; lens <= s.size(); ++lens) {
    for(int lent = 1; lent < t.size(); ++lent) {
      dp[lens][lent] = s[lens-1] == t[lent-1] ? (1 + dp[lens-1][lent-1]) : 
        max(dp[lens][lent-1], dp[lens-1][lent]);
    }
  }
  cout << dp[s.size()][t.size()];
  return 0;
}
