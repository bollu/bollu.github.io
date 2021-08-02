#include <atomic>
#include <cstdio>
#include <limits>
#include <map>
#include <set>
#include <iostream>
#include <vector>
#include <queue>
#include <assert.h>
using namespace std;
// Jewels of Stringology, Chapter 4: on-line construction of suffix trees.
// if node u <-> suffix s[i:] u, then node suflink[u] <-> suffix s[i+1:].
// by convention, suflink[root] = root
// p[:i] -> prefix of length i
// s[i:] -> suffix starting at index i.

// we try to build Trie(s[:1]), Trie(s[:2]), all the way up to Trie(s[:n]).
// call nodes that correspond to suffix of s[:i] as the **essential nodes**
// at step `i`, since these are the nodes which are essential to growing the trie.

// "abaabb"

// 0. ""
// @ 
// we have suffixes ("")

// 1. "a"
// @-A-@ (@ is essential node -- it ends suffixes of the current prefix)
// we have suffixes ("a", "")

// 2. "ab"
//  @-A-o-B-@
//  |
//  |-B--@
// we have suffixes ("ab", "b", "") 

// 3. "aba"
//  @-A-@-B-o-A-@
//  |
//  |-B--o--A--@
// we have suffixes ("aba", "ba", "a", "")


// 4. "abaa"
//    o--A--@
//    |
//    | 
//  @-A-@-B-o-A-o-A-@
//  |
//  |-B--o--A--o--A--@
// we have suffixes ("abaa", "baa", "aa", "a", "")

// Suffix links: 

// 2. "ab"
//  @-A-o-B-@--
//  |         |
//  |-B--@ <--|suflink[2]


// 3. "aba"
//  @-A-@-B-o-A-@ >-
//  |   ^          |
//  |   | suf:2->1 |
//  |    -------   |
//  |          |   |
//  |          ^   | 
//  |-B--o--A--@ <-suf:3->2

// 4. "abaa"            suf:3->2 [THIS ONE CHANGED FROM PREV STEP suf:2->1]
//    o--A--@  <----------------------+
//    |     v                         |
//    |     |suf:2->1                 |
//    | +---+                         |
//    | |                             |
//    | v                             |
//  @-A-@-B-o-A-o-A-@ >--+            |
//  |                    |            |
//  |                    |            |
//  |                    |            |
//  |                    |            |
//  |                    |            |
//  |                    | suf:4->3   |
//  |-B--o--A--o--A--@ <-+            |
//                   v                |
//                   |                |
//                    ----------------+

struct Node {
	static const int ALPHASIZE = 26;
	Node *next_[ALPHASIZE];
	Node *sufsmol = nullptr; 
	Node *sufbig = nullptr;
	Node() {
		for(int i = 0; i < ALPHASIZE; i++) {
			next_[i] = nullptr;
		}
	}

	// Node(const Node &other) {
	// 	sufsmol = other.sufsmol;
	// 	sufbig = other.sufbig;
	// 	for(int i = 0; i < ALPHASIZE; i++) {
	// 		next_[i] = other.next_[i];
	// 	}
	// }

	static void suffixLinkBig2Smol(Node *&big, Node *&smol) {
		smol->sufbig = big;
		big->sufsmol = smol;
	}

	Node *&next(char c) {
		return next_[c - 'a'];
	}
};

struct Trie {
	std::string s;
	Node *root;

	Trie(std::string s) : s(s) {
		root = new Node();
		// Node *c0 = new Node();
		// root->next(s[0]) = c0;
		// Node::suffixLinkBig2Smol(c0, root);
		// Node *deepest = c0;
		Node *deepest = root;
		cout << "\t-root |" << root << "|\n";
		// cout << "\t-c0 |" << c0 << "|\n";

		for(int startix = 0; startix < s.size(); ++startix) {
			vector<Node *> newsuffixes;
			const char c = s[startix];
			cout << "\t-adding new nodes for start ix [" << startix << "]" << c << "...\n";
			for(Node *cur = deepest; cur != nullptr; cur = cur->sufsmol) {
				cout << "\t\t-at node |" << cur << "|\n";
				if (!cur->next(c)) {
					cur->next(c) =  new Node();
				}

				assert(cur->next(c) && "expected next node");
				cout << "\t\t-at node |" << cur << "|. next node |" << cur->next(c) << "|\n";
				newsuffixes.push_back(cur->next(c));
			}

			newsuffixes.push_back(root); // empty string.

			cout << "\t-DONE new nodes [" << startix << "]" << c << ". Adding Suffix Links...\n";

			for(int i = 0; i +1 < newsuffixes.size(); ++i) {
				cout << "\t\t-adding suffix link at |" << i << "| "
						  <<  "big(|" << newsuffixes[i] << "|) -> smol(|"  << newsuffixes[i+1] << "|)\n";
				Node::suffixLinkBig2Smol(newsuffixes[i], newsuffixes[i+1]);
				cout << "\t\t-DONE adding suffix links\n "; 

			}
			assert(newsuffixes.size() >= 1);
			deepest = newsuffixes[0];
			cout << "\t-DONE suffix links.\n";

		}
	}
};

void check_trie(const Trie &t) {
	cout << "Checking trie built for string |" << t.s << "|\n";
	set<Node *> visited;
	vector<Node *> sfxsBigToSmol;

	for(int start = 0; start < t.s.size(); ++start) {
		Node *cur = t.root;
		visited.insert(cur);
		for(int i = start; i < t.s.size(); ++i) {
			if (!cur->next(t.s[i])) {
				std::cerr << "unable to find node in trie for string |" << t.s << "|" <<
					" for suffix |" << t.s.c_str() + start << "| at index |" << i << "(" << t.s[i] << ")" << "|\n";  
			}
			assert(cur->next(t.s[i]));
			cur = cur->next(t.s[i]);
			visited.insert(cur);
		}
		sfxsBigToSmol.push_back(cur);
	}
	
	// for(int i = 0; i < sfxsBigToSmol.size()-1; ++i) {
	// 	assert(sfxsBigToSmol[i]->sufbig == sfxsBigToSmol[i+1]);
	// 	assert(sfxsBigToSmol[i+1]->sufsmol == sfxsBigToSmol[i]);
	// }

	// check that all nodes in the trie correspond to substrings we find when
	// walking the trie using suffixes.
	queue<pair<Node *, string>> bfs;
	bfs.push({t.root, ""});
	while (!bfs.empty()) {
		Node *cur;
		string curstr;
		tie(cur, curstr) = bfs.front(); bfs.pop();
		if (!visited.count(cur)) {
			cout << "\t\t-unable to find string |" << curstr << "| as a string that should be on the trie.\n";
		}

		assert(visited.count(cur));
		for(int i = 0; i < Node::ALPHASIZE; ++i) {
			Node *next = cur->next_[i];
			string nextstr = curstr;
			nextstr += i + 'a';
			if (!next) { continue; }
			bfs.push({next, nextstr});
		}
	}
};

// String generation

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

// main

int main() {
  const int K = 3;
  const int NDIGITS = 6;
  const char *substr = "abc";
  for (int i = pow(K, NDIGITS); i < pow(K, NDIGITS + 2); ++i) {
    std::string s = basek(i, K);
    cout << "Building trie for string |" << s << "|\n";
    Trie t(s);
    check_trie(t);
  }
  return 0;
}
