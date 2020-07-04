# Distributed systems

- Gerard Tel's Intro to distributed systems is my favourite book on
  the subject so far, due to it's precise mathematical descriptions of
  everything.

## Class Topics

##### Clocks:
- Scalar Time
- Vector Time
- Singhal-Kshemkalyani’s Differential Technique
- Matrix Time
- Virtual Time

##### Global State: 
- Consistent state
- Cuts
- Chandy Lamport algorithm
- Lai-Yang algorithm
- Mattern’s algorithm
- Acharya-Badrinath algorithm 

##### Mutual Exclusion: 

- Requirements, Metrics
- Lamport’s algorithm
- Ricart agrawala algorithm
- Maekawa algorithm
- Suzuki Kasami algorithm
- Raymond’s algorithm

##### Deadlock Detection:
- Wait-For-Graph(WFG)
- Deadlock Handling strategies and issues
- Deadlock detection - issues, correctness criteria and resolution
- Models of deadlock - Single Resource Model, AND model, OR model, AND-OR model, (qp) model, Unrestricted model
- Knapp’s Classification - Path-pushing algorithms, Edge-chasing algorithms, Diffusing Computations Based Algorithms, Global State Detection Based Algorithms
- Chandy-Misra-Haas Algorithm for the AND Model
- Chandy-Misra-Haas Algorithm for the OR Model
- Ho-Ramamoorthy Algorithm
- Obermarck's Path-Pushing Algorithm
- Mitchell and Merritt’s Algorithm for the Single-Resource Model

##### Termination Detection:

- System Model and definition of termination detection
- Algorithm - Termination detection by Weight Throwing

##### Wave and Traversal Algorithms:

- Wave algorithm - properties and an example: the Echo algorithm
- Traversal algorithms - properties and an example: Sequential Polling
- Classical Depth-first Search
- Awerbuch’s DFS Algorithm
- Cidon’s DFS Algorithm

##### Routing Algorithms:
- Features and Issues
- Different measures of a good path
- Destination-based Forwarding
- Floyd-Warshall Algorithm - the simple distributed algorithm and Toueg’s improvement
- Chandy-Misra Algorithm
- Netchange Algorithm

##### Minimal Spanning Tree:
- Definition, Properties
- the notion of a fragment 
- Gallager-Humblet-Spira Algorithm

##### Agreement Protocols:
- Classification of Faults and Tolerance
- Core problems faced in distributed systems
- Overview of Consensus Results with crash failures/Byzantine Failures
- Consensus Algorithm for Crash Failures
- Byzantine Agreement and variants - Consensus, Interactive Consistency
- Proof of no t-Byzantine-robust broadcast protocol for t ≥N/3
- Lamport‐Shostak‐Pease Algorithm
- Asynchronous renaming - wait free renaming

##### Presentation Topics
- Omega and Butterfly Network
- How does scalar time deal with out of order messages
- Model Of Distributed Systems
- Types of Systems(coupling)
- History of Distributed Systems
- Requirements of Distributed Systems
- NTP synchronisation algorithm
- Types of Failures
- Compare client-server and P2P architecture
- Remote Procedure Calls - working
- Three levels of topology abstractions
- Scalability & Scalability testing
- Compare horizontal and vertical scalability
- What is database sharding
- Advantages and disadvantages of sharding
- SQL vs NoSQL in terms of scalability
- What is virtual time(Characteristics)
- Implementation of vector clocks
- How can kshemkalyani for global snapshot be modified for multiple initiators
- Chord Protocol
- Singhal’s Dynamic Information-Structure Algorithm - Introduction, Challenges and System Model
- Singhal’s Dynamic Information-Structure Algorithm - Algorithm, correctness
- Tree Structured Quorums in Agarwal-El Abbadi algorithm for ME
- hdfs architecture
- Why is deadlock avoidance strategy not used in distributed systems
- what is Gfs
- Sun Network File system
- deadlock detection in the AND-OR model
- Adrew File System
- Spanning Tree based Termination Detection
- Termination detection using distributed snapshots
- Compare distributed deadlock detection strategies
- Load Balancing - Benefits and Methods
- Synchronous single initiator spanning tree algo using flooding
- Asynchronous single initiator spanning tree algo using flooding
- Asynchronous concurrent initiator spanning tree algo using flooding
- Synchronous distributed bellman-ford
- Asynchronous distributed bellman-ford
- Synchronous GHS algorithm for MST
- Luby's Algorithm for Maximal Independent Set
- Synchronizers - Introduction, alpha synchronizer
- Beta and gamma synchronizer
- Leader Election - Bully Algorithm with example
- Leader Election - Chang-Roberts algorithm
- MapReduce
- 2 Phase Commit
- 3 Phase Commit
- Raft consensus algorithm
- pBFT


$$
\begin{array}{|c|c|c|c|}
Algorithm name & Domain & Type & Key insight \\
Chandy Lamport  & unk & unk & unk \\
Lai Yang  & unk & unk & unk \\
Mattern& unk & unk & unk \\
Acharya Badrinat & unk & unk & unk \\
Lamport (Mutex) & unk & unk & unk \\
Ricart Agarwala (Mutex) & unk & unk & unk \\
Maekawa & unk & unk & unk \\
Token-based Mutex & unk & unk & unk \\
Path Pushing (Obermack) & unk & unk & unk \\
Edge Chasing (Chandy Misra Haas) & unk & unk & unk \\
Deadlock - Mitchell Merritts & unk & unk & unk \\
Wave  Traversal algorithms & unk & unk & unk \\
Echo Wave & Wave & unk & unk \\
Cidon's DFS & Traversal & unk & unk \\
Routing - All-pairs-shortest-path & unk & unk & unk \\
Min Spanning tree (GHS)& unk & unk & unk \\
Agreement Protocols & unk & unk & unk \\
Lamport Shostak Pease & unk & unk & unk \\
\end{array}
$$
# Clocks

# Global State: 
<!-- https://www.cs.uic.edu/~ajayk/Chapter4.pdf -->
$n$ processors connected by channels. $LS(i)$ is the local state of
process $i$. $C(i, j)$ is the channel from process $i$ to process $j$, whose
state is $SC(i, j)$.
a message $send(m(i, j)), recv(m(i, j))$ be the send and receive
events corresponding to message $m$. We define messages in transit
as
$transit(LS(i), LS(j)) \equiv \{ m(i, j) : send(m(i, j)) \land \lnot recv(m(i, j)) \}$.

#### Global state, Consistency

$$
GS \equiv \{ \cup_i LS(i), \cup_{i, j} SC(i, j) \}
$$

A global state is consistent if the follows the following two conditions:
1. A message that is sent must either be in flight or must be received, not both: 
   $send(m(i, j)) \in LS(i) \implies  m(i, j) \in SC(i, j) \oplus rec(m(i, j)) \in LS(j)$
2. A message that has not been sent cannot be in-flight or have been
   received: $send(m(i, j)) \not \in LS(i) \implies m(i, j) \not \in SC(i, j) \land rec(m(i, j)) \not \in LS(j)$ 


## Cuts
- On each process line, choose points. Connect all these points. This defines
  PAST versus FUTURE.
- A **consistent cut** is one where we enforce the fact that a message
  must have been sent before it was received. Thus a message received in the
  past must have been sent in the past. If a message was received in the future,
  it might have been sent from the past, or in the future.
- Such a cut is called as consistent cut.

## Issues recording global state


1. Distinguishing which messages need to part of snapshot.
2. How to determine _when_ process should take snapshot.

## Chandy Lamport algorithm

- After a site has snapshooted itself, it sends a marker along all of its
  channels. In a FIFO system, this separates past and future.

```py
class ChandyLamport:
  def __init__(self): pass
  def send_marker(self):
    self.state_snapshot = copy.deepcopy(self.state) # snapshot of state
    for n in neighbours: n.send(Marker())
  def __recv__(self, Marker):
    if not self.snapshot:
      # Record the state of C as the empty set
      # Follow the “Marker Sending Rule”
    else:
      # Record the state of C as the set of messages
      # received along C after j’s state was recorded
      # and before j received the marker along C
        

```


## Lai-Yang algorithm

- Colors computations to allow for marker without needing FIFO.
- Markers piggybacked on computation messages. Message history required to compute channel states.
- Every process is initially white. Turns red on taking snapshot. Marker sending
  rule is run on taking snapshot.
- Every message is colored by the color of the process. White processes send
  white messages; red processes send red messages.
- Each white process takes a snapshot whenever it wants, but _no later_
  than the instant it receives a red message.
- Each white process records a history of all white messages sent
  or received by it on each channel.
- When it turns red, it send histories + snapshot to the
  initiator [how does it know who the initiator is? presumably the redding
  message carries this information].
- the initiator process evaluates $transit(LS(i), LS(j))$ to compute the
  state of a channel $C(i, j)$. $SC(i, j)$ is the white messages sent by
  $p_i$ on $C(i, j)$, that have \emph{not yet been received} by $p_j$.
  so it's $\{ m(i, j) : send(m(i, j)) \in LS_i \land \not rec(m(i, j)) \in LS(j) \}$.

## Mattern’s algorithm
- Based on vector clocks, single initiator. Termination detection needed.
- Initiator picks a future vector time $\mathbf{s}$ when they would like the
  global snapshot to be recorded. $s$ is broadcasted , after which it freezes
  all activity till it receives all acknowledgements of the receipt of
  this broadcast.
- Upon receiving the broadcast, other processes remember the value of $s$ and sends
  and ack to the initiator.
- The initiator on receiving ack from all processes, increases its vector clock
  to $s$, and sends a `DUMMY` message to all processes.
-  The recipients of `DUMMY` messages forces their clocks to go to a value
   ($\geq s$) if it was not already ($\geq s$).
- Each process takes a local snapshot and sends it to the initiaor when their
  clocks go from (less than $s$) to (greater than or equal to $s$).
- The state of $C(i, j)$ is the set of all messages that have been sent along
  $C(i, j)$, whose timestamp is smaller than $s$, which have been received by $p_j$
  after recording $LS(j)$.

#### Termination detection in Mattern's algorithm: Method 1
#### Termination detection in Mattern's algorithm: Method 2


## Acharya-Badrinath algorithm 

This is used for snapshotting in causally ordered systems.
- It beings by the initiator broadcasting a `TOKEN` to everyone. Let the copy
  of `TOKEN` received by $p_i$ be denoted as `TOKEN[i]`.
- A process records their local snapshot on receiving `TOKEN[i]` and sends
  the snapshot back to the initiator.
- Algorithm terminates when we have received snapshot from all processes.

#### Correctness: intuition.
We prove that:

$$
send(m(i, j)) \not \in LS(i) \implies rec(m(i, j)) \not \in LS(j)
$$

We assume that $send(m(i, j)) \not in LS(i)$. This means that
$recv(TOKEN(INIT, i)) \rightarrow send(m(i, j))$, since the snapshot
contains all messages before token was received.

Cases matrix:  Two choices for sending tokens:
1. $send(TOKEN(INIT, j)) \rightarrow send(TOKEN(init, i))$. 
2. $send(TOKEN(INIT, i)) \rightarrow send(TOKEN(init, j))$. 

Cases matrix:  Two choices for receiving token at $j$:
1. $recv(TOKEN(init, j)) \rightarrow recv(m(i, j))$: We are safe, $m(i, j)) \not \in LS(j)$.
2.  $recm(m(i, j)) \rightarrow recv(TOKEN(init, j))$. We should prove this is impossible.

##### Case 1:

- $send(TOKEN(INIT, j)) \rightarrow send(TOKEN(init, i))$. [Case 1]
- $recv(TOKEN(INIT, i)) \rightarrow send(m(i, j))$. [Given]

We can build a chain:

$$
send(TOKEN(INIT, j)) \rightarrow send(TOKEN(INIT, i)) \rightarrow recv(TOKEN(INIT, i)) \rightarrow send(m(i, j))
$$

here, it is clear that since $send(TOKEN(INIT, j)) \rightarrow send(m(i, j))$, 
we must have $recv(TOKEN(INIT, j)) \rightarrow recv(m(i, j))$, and hence
$rec(m(i, j)) \not \in LS(j)$.

##### Case 2:

- $send(TOKEN(INIT, i)) \rightarrow send(TOKEN(init, j))$. [Case 2]
- $recv(TOKEN(INIT, i)) \rightarrow send(m(i, j))$. [Given]
- $recv(m(i, j)) \rightarrow recv(TOKEN(init, j))$. [Taken for contradiction]

Since  $recv(m(i, j)) \rightarrow recv(TOKEN(init, j))$, by causal orderinging,
we must have that $send(m(i, j)) \rightarrow send(TOKEN(init, j))$.

**TODO: I am confused.**


# Deadlock detection

## Wait-For-Graph(WFG)

Nodes are processes. Edge from $P_1$ to $P_2$ if $P_1$ is blocked and is
waiting for $P_2$ to release some resource. A system is deadlocked iff there
is a cycle in the WFG.

## Deadlock Handling strategies and issues

We can perform deadlock prevention, deadlock aviodance, and deadlock detection.

- **deadlock prevention:** a process must gain all resources before it enters CS.
- **deadlock avoidance:** a resource is granted to a process if the global
  state is safe. This cannot be done sanely because the global state is far too
  expensive to compute.
- **deadlock detection:**

## Deadlock detection - issues, correctness criteria and resolution


## Models of deadlock - Single Resource Model, AND model, OR model, AND-OR model, (qp) model, Unrestricted model
#### Single resource
Can only request a single resource
#### AND model

Can request AND of multiple resource. Multiple resources can be asked for at
once. Request is satisfied after all the resources are given.

A cycle in the WFG imlpies a deadlock.

A deadlock need not always create a cycle. You can have processes that are
waiting on a deadlock be deadlocked, though they are outside the cycle --- ie,
they are part of the knot.

#### OR model

Can request OR of multiple resources. Request is satisfied if any resource
is given.

#### AND-OR model

Can ask for any boolean combination made by `AND` and `OR`.

#### $\comb{p}{q}$ model

allows us to request to get $p$ resources from a pool of $q$ candidate resources.
For example, in the AND model, we have $p$ resources and we want to get all $p$
of them, so this is $\comb{p}{p}$. Similarly, in the OR model, we want to get
_one_ of some $p$ requests. So this is $\comb{p}{1}$.

## Knapp’s Classification - Path-pushing algorithms, Edge-chasing algorithms, Diffusing Computations Based Algorithms, Global State Detection Based Algorithms

#### Path pushing

Maintain explicit global WFG. we send local WFG to other sites. eventually
someone will have large enough picture to see the deadlock.

#### Edge chasing

A cycle is detected by spreading around probe messages in the graph. If a site
receives a matching probe that it has sent, then it has detected an edge.

Probes are fixed sized, hence short.

#### Diffusing computation

echo algorithms are used to detect deadlocks. If the computation terminates,
initiator declares deadlock.


## Chandy-Misra-Haas Algorithm for the AND Model
## Chandy-Misra-Haas Algorithm for the OR Model
## Ho-Ramamoorthy Algorithm
## Obermarck's Path-Pushing Algorithm
## Mitchell and Merritt’s Algorithm for the Single-Resource Model

# Termination detection

## Weight throwing termination detection

- $p_i(t)$ is the state (active or idle) of process $p_i$ at time $t$.
- let $c_{i, j}(t)$ denote the number of messages in transit at time $t$
  from process $i$ to process $j$.
- A computation has terminated if $p_i(t) = idle$ and $c_{i, j}(t) = 0$
  for all $i, j$.
- Bidirectional communication, reliable but non-FIFO.
- Weight of each process is $0$. Weight of controlling agent is $1$.
- Computation starts when controlling agent sends basic message to one 
  of the processes.
- A nonzero weight $0 \leq 0 \leq 1$ is assigned to each process in the
  active state and each message in transit.
- When computation starts, controlling agent has all the weight. It shares
  some weight with the process it asks to start.
- Processes share weight through the messages they send: sender loses weight,
  message has weight in-flight, receiver gains weight once message is
  received.
- When a process has terminated, it returns its weight to the controller.
- Once all processes are done, the controller will have weight 1 again.



# 12: Wave and traversal

## Wave algorithms

Processes are in set $P$. Events in computation are in a set $C$. Events
of each process $p$ is called $C_p$. Events are ordered by the precedence
relation $(\leq)$.

- **Termination**: Each computation is finite: $|C| < \infty$.
- **Decision** :Each computation has at least one decide event $\exists decide \in C$.
- **Dependence**: Each decide event is causally preceded by an event:

$$
\forall e \in C, \text{$e$ is a decide event} \implies \forall q \in P, \exists e_q \in C_q, e_q \leq e
$$

### Echo Algorithm

Initiator:

```hs
send :: Id -> t -> Dis s t
recv :: (t, s) -> Dis s t

-- | list of neighbours
neighbours :: [Id]; neighbours = ...

data Tok = Tok -- ^ data sent on wire

init :: Dis () Tok
init = forM_ neighbours (\n -> send n Tok) 

recv :: (Int, Tok) -> Dis Int Tok
recv (Tok, i) = let i' = i + 1 in
  if i' == length neighbours then decide else return i'

code :: [Id] -> Dis Int State
code ns = mkAgent 0 init recv
```

non-initiator:

```py
class Recv:
    def __init__(self): self.nrecv = 0; self.father = None;
    def recv(self, sender, token):
        if not self.father: self.father = sender;
        self.nrecv += 1;
        if self.nrecv == len(self.neighbours): 
            self.father.send(Tok())
```


## Traversal algorithm

- One initiator. Starts by sending out one message.
- A process on receiving a message sends out one message or decides.
- Algorithm teminates _in the initiator_, when each process
  has sent a message at least once.

So intuitively, there is a _token_ that is handed from process to process.

### Sequential Polling

```py
class Initiator:
    def __init__(self):
        self.recv = 0;
        while self.recv < len(self.neighbours):
            def callback(tok): self.recv += 1
            self.neighbours[self.recv].sendBlocking(Tok(), callback)

class NonInitiator:
    def __init__(self): pass
    def recv(self, neigh, tok): neigh.send(Tok())
```

### Classical DFS

```py
class Initiator:
    def __init__(self):
        self.father = self
```

### Awerbuch's DFS

```py
VIS = 0; ACK = 1; TOK = 2;
class Awerbuch:
    def __init__(self): self.father = None; self.is_initiator = False;
        self.used = {n: False for n in self.neighbours}

    def recv(self, START):
        self.is_initiator = True;
        self.father = self;
        q = choice(self.neighbours)
        for n in self.neighbours: n.send(VIS)
        for n in self.neighbours: n.receiveBlocking(ACK)
        self.used[q] = True; q.send(TOK)

    def recv(self, sender, VIS): self.used[sender] = True; sender.send(ACK)

    def recv(self, sender, TOK):
        if not self.father:
            self.father = sender
            for n in neighbours: if n != self.father: n.send(VIS)
            for n in neighbours: if n != self.father: n.receiveBlocking(VIS)

        if self.is_initiator and all(self.used): self.decide()

        nfree = [n for n in self.neighbours if \
                  n != self.father and not self.used[n]]
        if nfree: next = nfree[0]; self.used[next] = True; next.send(TOK)
        else: self.used[self.father] = True; self.father.send(TOK)
```


### Cidon's DFS

- [yet another distribu1ed depth-first-search algorithm](http://www.hansdieterhiep.nl/2017/05/Cidon.pdf)
- Uses $2|V|$ units of time, $3|E|$ units of communication.
- FIFO is *not* needed.
- Graph is undirected.
- All messages arrive in arbitrary but finite time, not necessarily in the
  same order (not FIFO).
- A node knows all of its links, and the identity of the link over
  which the message arrived.
- Token is sent from node to node.
- **Key idea:** each node, upon receiving the token forthe first time, 
  notifies its neighbors that it hasbeen visited.

#### Difference from Awerbuch

In awerbuch, an acknowledgement is sent for each
notification and the node holds the token until all notifications are acknowledged.
In this algo, **no ack is used**, so no time is spent waiting for
acknowledgements. Rather, the token is forwarded immediately; The token
maybe sent to an already explored node. The fact that the message
arrived later than the first message (which explored the node).

#### Algorithm

```py
SIDLE = 0; SDISCOVERED = 1; # states
EUNVISITITED = 0; EVISITED = 1; EFATHER = 2; ESON = 3; # edges
MSGVISITED = 0; MSGTOKEN = 1; MSGSTART = 3
class Cidon:
    def __init__(self): 
        self.state = SIDLE;
        self.mark = { n : EUNVISITED for n in self.neighbours};

    def recv(self, START):
        self.state = SDISCOVERED;
        self.search()
        for n in self.neighbours: 
            if self.mark[n] in [EVISITED, EUNVISITED]: n.send(MSGVISITED);

    def recv(self, sender, MSGTOKEN):
        if self.state == SIDLE:
            self.mark[sender] = EFATHER; self.state = SDISCOVERED;
            search();
            for n in self.neighbours: 
                if self.mark[n] in [EVISITED, EUNVISITED]: n.send(MSGVISITED)
        else:
            assert(self.state == SDISCOVERED)
            assert (self.mark[sender] in [UNVISITED, SON])
            if self.mark[sender] == UNVISITED: self.mark[sender] = VISITED
            elif self.mark[sender] == SON: self.search()
    
    def recv(self, sender, VISITED):

    def search(self):
      unvisiteds = [n for n in self.neighbours if self.mark[n] = EUNVISITED]
      if unvisiteds:
          k = unvisiteds[0]; k.send(MSGTOKEN); self.mark[k] = ESON;
      else:
        if self.is_source(): self.stop()
        else: 
            # we are sure that such a node exists.
            f = [n for n in neighbours if self.mark[n] = EFATHER][0]
            f.send(TOKEN)
```
  


# 13: Routing

Each site maintains a local wait-for graph. Central site begins a check.
All of the data is pushed to central node which begins the check.

## All Pairs Shortest Path Algorthms

- Content from distributed algorithms by gerard tel: Chapter 4

When a packet is sent in a network, it should be forwarded using routing tables.
The routing table must be computed when the network is initialized, and must
be updated every time the topology of the network changes.

We assume that the network is undirected, and we have no negative weight
cycles.  Our weights are typically congestion.

- *Correctness*: Algorithm must deliver every packet to its ultimate destination.
- *Robustness*: If the topology changes, the algorithm should update its
  routing table accordingly.

## Floyd Warshall for routing table:

Perform matrix multiplication on the `(min, +)` semiring.

```py
def floyd_warshall(V, E):
    UnseenV = set(V)

    W = np.fill("infty", shape=[len(V), len(V)]) -- distances
    B = [[None for _ in len(V)] for _ in len(V)] -- intermediate paths.
    for (u, weight, v) in E: W[u][v] = weight; B[u][v] = v;

    while UnseenV:
        w = UnseenV.randitem(); UnseenV.delete(w);
        if W[a][w] + W[w][b] < W[a][b]:
            B[a][b] = B[a][w] -- if a m
            W[a][b] = W[a][w] + W[w][b]
```

#### The simple distributed Floyd Warshall Algorithm:

```py
# nth processor  is running the code floyd_warshall(n, V, E)
def floyd_warshall(n, V, E):
    UnseenV = set(V)

    W = np.fill("infty", shape=[len(V), len(V)]) -- distances
    B = [[None for _ in len(V)] for _ in len(V)] -- intermediate paths.
    for (u, weight, v) in E: W[u][v] = weight; B[u][v] = v;

    while UnseenV:
        # the ordering by which w's will be selected is _deterministic_,
        # and is _shared across all nodes_
        w = UnseenV.smallestitem(); UnseenV.delete(w);
        assert(w is the same across all nodes)

        if w == n: # if we are the current node
            broadcast(W[w, :]) # broadcast the weights of w
        else:
            W[w, :] = receive() # receive the updated weights of w

        if W[a][w] + W[w][b] < W[a][b]:
            B[a][b] = B[a][w] -- if a m
            W[a][b] = W[a][w] + W[w][b]
```

- If `NB[][w] = u`, then is `NB[u][w] = a`?


#### The simple distributed Floyd Warshall Algorithm, Toueg's version

Here, we are trying to define how to broadcast the table `W[w, :]`. It is
now `W`'s turn to broadcast.

Define a graph $G_w \equiv (V_w, E_w)$.

- $u \in V_w \iff D[u][w] < \infty$: if a path from $u \xrightarrow{*} w$ exists.
- $(u, x) \in E_w \iff u \neq d \land B[u][w] = x$: If a path from $u \xrightarrow{*} w$
  must begin with x: $u \rightarrow x \xrightarrow{*} w$.

We claim that is $G_w$ is in fact a tree. This is because every edge $(u, x)$
in fact signals a path from $u \rightarrow x \xrightarrow{*} w$. Hence,

This graph is connected since we only add vertices that are at distance
less than infinity.

Hence, $G_w$ is a connected directed acyclic graph, so a tree.


## Bellman Ford  (Single source shortest path algorithm)

- We have a root node $i_0$ whose shortest path we are trying to discover.
- Each vertex $k$ decides $d(k, i_0) = \min_{n \rightarrow k} d(k, n) + d(n, i_0)$.
- This is a dataflow algorithm!
- Once we actually know the single-source shortest paths, we can form a tree
  of shortest-path edges.
- At timestep $i$, all nodes at distance $i$ in the shortest path tree will
  indeed have learnt the shortest path.
- Since it's a tree, at most $|V| - 1$ non-trivial levels (level 0 is trivial).
  Hence, time complexity is $|V| - 1$.


## Chandy-Misra Algorithm: Similar to Bellman Ford


## Netchange algorithm for computing min-hop routing tables

Same as chandy-misra, but we allow edges to fail.
- We assume that nodes are notified failures and repairs of their
  adjacent channels.

- If we get a packet that is addressed to someone else, and our distance to
  that node is not infinity, then we send it to our first hop.

- If we get a packet that is addressed to someone else, and our distance to
  that node is infinity, discard.


- Let shortest path from $z \rightarrow w$ be through $u$ as $z \rightarrow u \rightarrow w$.
  Suppose chanell $uw$ fails. $u$ recomputes distance to $w$ as $d(w) = 1 + d(z, w) = 1 + 2 = 3$.
  Then it sets $nb(w) = z$. Is this incorrect?

- __ANSWER:__ No, it's not incorrect. Now $z$ will be invalidated and it'll attempt
  to recompute its distance, causing its dinstance to go higher (say 5).
  This invalidates $u$, leading to an increasing sequence of distances $3 \rightarrow 5 \rightarrow 7 \dots$.
  If we have an uppoer bound on the distance (`DIAMETER`) then we know that
  this is wrong.

# Minimal Spanning trees

#### Gallager-Humblet-Spira Algorithm

- [Link to algo (IIT Kanpur)](https://cse.iitkgp.ac.in/~pallab/dist_sys/Lec-8a-MinSpanningTree.pdf)
- [Link to paper](https://www.cs.tau.ac.il/~afek/p66-gallager.pdf)


- Fragment: subtree of MST.
- Extending a Fragment: [f:Fragment + {edge of lowest weight of f} = f'Fragment]
- Kruskal's: Starts with collection of fragments; merges till only one remains.

In Gallager-Humblet-Spira, we distribute Krusal's. Two assumptions:
1. Edge node has unique edge weight $w(e)$.
2. All nodes initially asleep. Wake up before algo execution. When a process
   is woken up, it first executes initialization, then processes the message.

##### Algorithm Outline

1. Start with each node as fragment.
2. Nodes in fragment find lowest outgoing edge.
3. When lowest outgoing edge is found, fragment is combined with. 
   another fragment by adding the outgoing edge.
4. Algorithm terminates when only one fragment remains.

##### Notations, Definitions

1. Fragment Name: a GUID for each fragment.
2. Fragment levels: Initially 0 for each fragment.
3. Smaller fragments combine w/ larger fragments by adopting {name, level} of
   larger fragment.
4. Fragments of the same level give a fragment that is one level higher.
   The new name of the fragment is the weight of the combining edge (which is
   guaranteed to be unique); This combining edge is called as the **core edge**
   of the fragment.

Formally, the rule of union of fragment $f \equiv (n, l)$ to invoke a join
with  $f' \equiv (n', l')$ where $n$
is the name, $l$ is the level, and the edge of lowest weight from $f$ to $f'$
is $e_l[f, f']$ ($l$ for lowest, $f, f'$ for the direction: from $f$ to $f'$):

$$
f \xrightarrow{e_l[f, f']} f' \equiv
(n, l) \cup (n', l') \equiv 
\begin{cases}
n' & l < l' \\
(($w(e[f, f']), l+1)$ & l = l' \\
\text{disallowed} & l > l'
\end{cases}
$$

According to paper, above is wrong. Paper says:


> If L = L' and fragments F and F' **have the same minimum-weight outgoing edge**,
> then the fragments combine immediately into a new fragment at
> level L + 1; the combining edge is then called the core of the new fragment

- We show later, after describing more of the algorithm, that the waiting in the
  above rules cannot cause a deadlock.

- The reason for the waiting is that the
  communication required for a fragment to find its minimum-weight edge is
  **proportional to the fragment size**, and thus **communication is reduced** by small
  fragments joining into large ones rather than vice versa. 

##### The algorit
hm

- `stach[p](q): branch | reject | basic`: process $p$ maintains information about the status of edge $(p, q)$.
  This is `branch` if edge is known to be in MST, `reject` if it is known not
  to be in MST, `basic` otherwise.

- `father[p]: Edge`: edge leading to the core edge of the fragment.

- `state[p]: sleep | find | found`: Initially in state `sleep`. Find `p`
  if currently searching for lowest-weight outgoing edge. `found` if
  already found.

###### the algebra of fragment fusion
A similar thing happens in clocks for example. When _I_ update my time,
ie, union(time me, time me), I get an element that's one up the lattice.
When I union with someone else, I get the max. So we have an algebraic structure
which is $(L, \leq, next: L \rightarrow L)$ where `next` is monotone for `(L, <=)`.
The  induced union operator $\cup: L \times L \rightarrow L$ is:

$$
x \cup y \equiv \begin{cases} next(x) & x = y  \\ max(x, y) & x \neq y \end{cases}
$$

- Is the total ordering on vector clocks *not* isomorphic to the total ordering on $\mathbb{R}$?

# Byazntine algorithms


#### Byzantine agreement:

Good reference for Pi protocol prooof:
[Easy impossibility proofs for distributed consensus problems](https://groups.csail.mit.edu/tds/papers/Lynch/FischerLynchMerritt-dc.pdf)


A process is _byzantine_ if it deviates from the specification.

# Consensus:

- Agreement: All non faulty processes must agree on the same value
- Validity: A decided value must be the proposed value if the initiator was not byzantine
- Termination: Every correct process returns a value

# Interactive consensus:

Each process outputs its own value $v_k$.
- All non-faulty processes agree on a vector $(v_1, v_2, \dots, v_n)$
- If the kth process is non-faulty and its initial value is $v_k$, then the
  kth value in the vector agreed upon up **all of the non-faulty processes**
  must have $v_k$ as the $k$th process.

## Core problems faced in distributed systems
## Overview of Consensus Results with crash failures/Byzantine Failures
## Consensus Algorithm for Crash Failures
## Byzantine Agreement and variants - Consensus, Interactive Consistency
-  http://www.cs.fsu.edu/~xyuan/cop5611/lecture11.html
## Proof of no t-Byzantine-robust broadcast protocol for $t \geq N/3$
## Lamport‐Shostak‐Pease Algorithm

$OM(#traitors, S)$ is the notation for the algorithms with those
many traitors with $S$ being the full set of processes.

#### Base case: $OM(0, S)$
This is easy. Commander sends commands, Lieutenants agree to command.

#### $OM(m, S)$ for $m > 0$

$S$ is the set of generals for which we want agreement.

- Commander $i$ sends value $v$ to every lieutenant $j$ in $S_{-i}$.
- For each lieutenant $j \in S_{-i}$, let $v(j)$ be the value received
  by the commander $i$, or else `RETREAT` if he receives no value.
  Lieutenant $j$ runs the algorithm $OM(m-1, S_{-i}$ recursively, acting
  as the commander, initiating with value $v(j)$.
- At the end of these recursive executions, all loyal lieutenants $j \in S_{-i}$
  have agreed on a set of pairs $Answer_j \equiv \{ (k, v_k) : k \in S_{-i} \}$.
- The result of Lieutenant $j$ for $OM(m, S)$ is the majority of $Answer_j$.


AKA 

## Asynchronous renaming - wait free renaming


# Snippets

### 5 minute talk: P2P
#### Napster
Napster connected to a central server than was an index of all users. Users are
connected to each other to download MP3 files off of each other.

#### BitTorrent

Each data segment is encrypted. Data segment is downloaded individually by
peers.

#### Chord protocol

Finger table: nodes are keys, contain values. A node's chord table is:
with ids `i :-> (n + 1^i)`.
