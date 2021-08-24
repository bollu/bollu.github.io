#include <assert.h>

#include <algorithm>
#include <bitset> // instead of bool[N];
#include <ext/typelist.h>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric> // for gcd
#include <queue>
#include <set>
#include <stack>
#include <stdlib.h> // for exit()
#include <vector>

// https://codeforces.com/blog/entry/11080
#include <ext/pb_ds/assoc_container.hpp> // Common file
#include <ext/pb_ds/detail/standard_policies.hpp>
#include <ext/pb_ds/tree_policy.hpp> // Including tree_order_statistics_node_update

using ll = long long;
using namespace std;

// min priority queue. Useful for djikstras, or in general, to be
// explicit.
template <typename T> using minqueue = priority_queue<T, vector<T>, greater<T>>;

template <typename T>
using ordered_set =
    __gnu_pbds::tree<T, __gnu_pbds::null_type, less<T>, __gnu_pbds::rb_tree_tag,
                     __gnu_pbds::tree_order_statistics_node_update>;

template <typename K, typename V>
using ordered_map =
    __gnu_pbds::tree<K, V, less<K>, __gnu_pbds::rb_tree_tag,
                     __gnu_pbds::tree_order_statistics_node_update>;

template <typename T1, typename T2>
ostream &operator<<(ostream &o, const pair<T1, T2> &p) {
  return o << "(" << p.first << ", " << p.second << ")";
}

// https://codeforces.com/blog/entry/90035

namespace automata {
struct Node {
  int len;    // length of longest string in equivalence class
  int minend; // minimum ending position of occurence

  Node *smol;
  map<char, Node *> beeg;
  Node() = default;
  Node(const Node &other) {
    this->len = other.len;
    this->minend = other.minend;
    this->smol = other.smol;
    this->beeg = other.beeg;
  }
};

vector<Node *> states;
Node *start = nullptr;
Node *end = nullptr;


vector<int> border;

void init() {
  start = new Node;
  start->smol = start;
  start->len = 0;
  start->minend = -1;
  end = start;
  states.push_back(start);
  border.push_back(0);
}

void extend(char c) {
  Node *cur = new Node();
  states.push_back(cur);
  cur->len = end->len + 1;
  cur->minend = cur->len - 1; // ending index.
  cur->smol = nullptr;        // to be discovered.
  border.push_back(0);

  Node *relink = end;
  end = cur; // update end.

  while (!relink->beeg.count(c)) {
     relink->beeg[c] = cur;
    if (relink == start) {
      cur->smol = start;
      return;
    } else {
        relink = relink->smol;
    }
  }

  // we tried to add [x+c], where x is a suffix of s.
  // such a state already exists.
  assert(relink->beeg.count(c));
  // relink -c-> q


  Node *q = relink->beeg[c];
  if (relink->len + 1 == q->len) {
    // [q] = [relink]:c
    cur->smol = q;
    return;
  } else {
    // [q] is longer that [x+c]
    // so [q] is of the form [x+c+delta]
    // [q] must be longer than [p] since q contains [x+c].
    // vvv TODO: why is it okay to not modify minend?

    Node *qsmol = new Node(*q);
    states.push_back(qsmol);
    qsmol->len = relink->len + 1;
    assert(qsmol->len < q->len);
    cur->smol = qsmol; // setup link.

    // q contains [p]:c
    // we have a state for [p]:c.
    // relink q to [p]:c.
    // vvv TODO: DEAR GOD WHY IS THIS CORRECT GOD DAMN IT?
    q->smol = qsmol;
    // relink all things smoler than p that used to point to p to point to
    // qsmol.
    while (relink->beeg.count(c) && relink->beeg[c] == q) {
      relink->beeg[c] = qsmol;
      if (relink == start) {
        return;
      } else {
        relink = relink->smol;
      }
    }
    return;
  }
}

int main() {
  std::ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  string s;
  cin >> s;
  init();
  // build automata for s+s.
  for (char c : s) {
    extend(c);
  }

  vector<int>border(s.size(), 0);
  set<Node *> terminals;
  Node *f = end;
  while(f != start) {
    terminals.insert(f);
    f = f->smol;
  }


  return 0;
}

}; // namespace automata
