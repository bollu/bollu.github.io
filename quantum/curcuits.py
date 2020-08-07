# Superdense coding

- (00 + 11) is shared
- Alice wants to communicate two bits of classical info to bob.
- To send 00, she applies I, making the state `00 + 11`
- To send 01, she applies Z to her qubit, making the state `00 - 11`
- To send 10, she applies X to her qubit, making the state `10 + 01`
- To send 11, she applies iY to her qubit, making the state `i(i10 -i01)` = `-10 + 01`

Now make a measurement in the given basis, with projection operators |00+11><00+11|,
|00-11><00-11|, |10+01><10+01|, |-10+01><-10+01|


# Teleportation

- Call the qubits s (for state) b0, b1 (for bell qubits).
- Initial state: `sb0b1 = (a0 + b1)(00 + 11)`.

- apply `CNOT([s], b0)` = a0(00 + 11) + b1(10 + 01).
- apply H(s) = a(0 + 1)(00 + 11) + b(0 - 1)(10 + 01).
- 00(a0 + b1) + 01(a1 + b0) + 10(a0 - b1) + 11 (a1 - b0).
- Now ask bob to fixup the circuit.

# Deutch Jozsa
- Function is either balanced (0 for half inputs and 1 for half inputs) or
constant (0/1 for all inputs)

- start with 0^n1.
- apply H^n. Gives state \sum_i=0^{2^n-1} x(0 - 1)
- apply U_f. Gives state \sum_i=0^{2^n-1} x(|0 XOR f(x)) - |1 XOR f(x))
- Note that f(x) = 0 or 1. So, the state can be written as:
- \sum_i=0^{2^n-1} (-1)^f(x) x (0 - 1)

- We now apply an H gate to the first n-1 qubits giving
- \sum_i=0^{2^n-1} (-1)^f(x) H(x) = \sum_{i=0}^{2^n-1} \sum_{j=0}^{2^n-1}

# Grovers
- |s> = H^n(x) 
- |w> = that number such that f(w) = 1, f(x != w) = 0
- U_s = 2|s><s| - I == grover diffusion operator
- Initialize the system to the state |s>
- Perform (U_f U_s)^n
- perform measurement. It will result in the eigenvalue \lambda_w
- Us = 2|s><s| - I
- Uw = I - 2|w><w| = |w><w| + |wperp><wperp> - 2|w><w| = |wperp><wperp| - |w><w| = reflect perpendicular to |wperp>
- Consider the plane spanned by |w> and |wperp> . The angle between
 |s> and |wperp> is <s|wperp> = \sqrt(N-1/N)
- The angle will be sin (theta/2) = 1/sqrt(N)
- Note that the operator U_w will cause a reflection along |wperp>.


# Simons
- (\sum_{x\in{0, 1}^n} |x>) (x) |0^n>

# Shor's
