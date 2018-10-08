Let's study abstract interpretation on an SSA like language with no mutation.

\begin{code}
import Algebra.Lattice

-- identifiers
newtype Ident = Ident Int

-- basic block IDs
newtype BBID = BBID Int

-- Expressions
data Expr = EAdd Expr Expr | EIdent Ident | EConst Int

-- assignment statements
data Assign = Assign Ident Expr

-- terminator
data Terminator = Branch BBID | BranchCond Expr BBID BBID

-- phi node 
data Phi = Phi [(Ident, Expr)]
data BB = BB BBID [Phi] [Assign] Terminator

-- first basic block in program is the entry block.
data Program = Program [BB]
\end{code}



\begin{code}
main :: IO ()
main = putStrLn "foo"
\end{code}
