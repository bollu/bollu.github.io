// suffix automata from scratch.
#include <iostream>
#include <vector>
#include <map>
using namespace std;

// state which contains longest string of length |strlen|.
struct state {
	map<char, int> next;
    int strlen; // length of largest substring in this equivalence class.
    int link = -1; // suffix link.
};

struct automata {
	vector<state> ss;
    int strlen;
    int last;
};

automata make() {
    automata a;
    a.ss.push_back(state());
    a.ss[0].strlen = 0;
    a.strlen = 0;
    a.last = -1;
};

void extend(automata &a, char c) {
    const int penult = a.last;
    a.last = a.ss.size();
    a.strlen++;
    a.ss.push_back(a.ss[penult]);

    a.ss[a.last].strlen = a.strlen; 

    // transition to last from previous last.
    // extend any state which corresponds to a state of the form `s[x:]`
    int back = penult;
    while(back != -1 && !a.ss[back].next.count(c)) {
        a.ss[back].next[c] = a.last;
        back = a.ss[back].link;
    }

    // TODO: maybe neater to create loop back=back to signal end?
    if (back == -1) { 
    	a.ss[a.last].link = 0; // smallest string not in equiv class of final
        // is empty string.
    	return;
    }

    assert(back != -1);

    // we have a state p such that p can transition using c to q.
    const int p = back;
    const int q = a.ss[back].next[c];

    // we can transition from longest string of p to an equiv. class containing
    // p + c. So either representative [p]+c is |p+c| or something even longer.
    // if it is indeed [p]+c = [p+c], then make fwd point to [p+c].
    if (a.ss[p].strlen + 1 == a.ss[q].strlen)  {
        a.ss[last].link = q;
    } else {
        // if not, create a new state for [p+c].
        int qshort = a.ss.size();
        a.ss.push_back(a.ss[q]);
        a.ss[qshort].strlen = a.ss[p].strlen+1;
        int qprev = p;
        while (qprev != -1 && a.ss[qprev].next[c] == q) {
            a.ss[p].next[c] = qshort;
            qprev = a.ss[qprev].link;
        }

        a.ss[back].link = qshort;

        // proof? Why can't there be a state between qshort and q?
        // suppose there was.
        // so q has suffix link to some other state q'.
        // by chopping, p has suffix link 
        a.ss[q].link = qshort;
    }

}
