// https://codeforces.com/blog/entry/20861
// https://en.wikipedia.org/wiki/Suffix_automaton#Automaton_structure

#include<map>
#include<assert.h>
using namespace std;

struct state {
    int len, link;
    map<char, int> next;
};


const int MAXLEN = 100000;
state st[MAXLEN * 2];
int sz, last;


// invariant: states in the suffix automata are equivalence
// classes of strings that end at all the same positions.
// endpos(t) = { set of positions where t ends as a substring of s }
// t1 ~ t2 iff endpos(t1) = endpos(t2).

// Claim: strings in the state of a suffix automata are all suffixes of each other.


void sa_init() {
    st[0].len = 0;
    st[0].link = -1;
    sz++;
    last = 0;
}

void sa_extend(char c) {
    int cur = sz++; // add new state
    st[cur].len = st[last].len + 1;

    int p = last; // add s[p]:c to last.
    while (p != -1 && !st[p].next.count(c)) {
        st[p].next[c] = cur;
		// travel to [~] equiv-class of smaller suffixes
        p = st[p].link; 
    }

	// we found no equiv. class we conflict with.
    if (p == -1) {
        st[cur].link = 0; // 0? or -1?
		last = cur;
		return;
    } else {
		// p has a next state c with c. call it q.
        int q = st[p].next[c];
		// **solid edge**: q comes right after p. link to it.
		// We have to make a suffix link to a state, in which the longest
        // string is exactly s:c, i.e. the len of q should be len(p)+1.
        if (st[p].len + 1 == st[q].len) {
            st[cur].link = q;
        } else {
			// q occurs somewhere much after p.
			// q corresponds not only to some suffix of |s:c| of len |len(p)+1|, but to some
			// longer suffix as well.
			assert(st[q].len > st[p].len+1);

			// create a new state |clone| for p:c, identical to q.
            int clone = sz++;
            st[clone].len = st[p].len + 1;
            st[clone].next = st[q].next; // keep paths that travel through q same.
            st[clone].link = st[q].link; // keep paths that travel through q same.
			// update all links that point from p (and its suffixes) to q
			// to point to clone.
            while (p != -1 && st[p].next[c] == q) {
                st[p].next[c] = clone;
                p = st[p].link;
            }
			// q contains p:c. Hence p:c is a suffix of q.
			// Hence link q to p:c.
			// TODO: why is this safe? Why can't st[q].link point to something larger?
            st[q].link = st[cur].link = clone;
        }
    }
    last = cur;
}



