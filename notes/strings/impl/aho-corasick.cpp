// aho-corasick.
struct Node {
    Node *suffix = nullptr;
    int parentDelta = 0;
    Node *parent = nullptr;
    Node *transitions[26];
    Node() {
        for(int i = 0; i < 26; ++i) { transitions[i] = nullptr; }
    }
};

Node *start;

void init() {
    start = new Node;
    start->parent = start;
    start->suffix = start;
}

void add(const string &s) {
    Node *cur = start;
    for(char c : s) {
        const int ci = c - 'a';
        if (cur->transitions[ci]) { cur = cur->transitions[ci]; }
        else {
            Node *next = new Node();
            next->parent = cur;
            next->parentDelta = ci;
            cur->transitions[ci] = next;

        }
    }
}

void calcSuffixes() {
    queue<Node *> bfs;
    for(int i = 0; i < 26; ++i) {
        Node *single = start->transitions[i];
        if (!single) { continue; }
        single->suffix = start;
        for(int j = 0; j < 26; ++j) {
            Node *next = single->transitions[i];
            if (next) { bfs.push(next); }
        }
    }


    while(bfs.size()) {
        Node *cur = bfs.front()
        assert(cur->parentDelta != 0);
        // longest-suffix(parent:c) = longest-suffix(parent):c
        assert(cur->parent);
        assert(cur->parent->suffix);
        cur->suffix = cur->parent->suffix->transitions[parentDelta];
        // if transition is illegal, set suffix link to start.
        if (!cur->suffix) { cur->suffix = start; }

        for(int i = 0; i < cur->transitions.size(); ++i) {
            if (!cur->transitions[i]) { continue; };
            bfs.push(cur->transitions[i]);
        }
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    return 0;
}
