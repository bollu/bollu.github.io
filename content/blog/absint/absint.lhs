http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/10.ABSTRACT-INTERPRETATION.html

Let's study abstract interpretation on an SSA like language with no mutation.

\begin{code}
import Algebra.Lattice
import qualified Data.Map.Strict as M
import Control.Monad.State 

-- identifiers
newtype Ident = Ident Int deriving(Eq, Ord, Show)

-- basic block IDs
newtype BBID = BBID Int deriving(Eq, Ord, Show)

-- Expressions
data Expr = EAdd Expr Expr | EIdent Ident | EConst Int

-- assignment statements
data Assign = Assign Ident Expr

-- terminator
data Terminator = Branch BBID | BranchCond Expr BBID BBID | Exit Expr

-- phi node 
data Phi = Phi [(Ident, Expr)]
data BB = BB { bbid :: BBID, bbphi :: [Phi], bbassigns :: [Assign], bbterm :: Terminator }

-- first basic block in program is the entry block.
newtype Program = Program [BB]

-- get the BB corresponding to BBID in Program p
programGetBB :: Program -> BBID -> BB
programGetBB (bb:bbs) needle =  
    if bbid bb == needle
    then bb
    else programGetBB bbs needle

-- environments refer to the entire program due to the nature of
-- our immutable language
type Env a = M.Map Ident a


data InterpState = InterpState {
    prevbbid :: Maybe BBID,
    curbbid :: BBID,
    env :: Env Int
}

-- create the initial interpreter state for a given basic block ID
mkInitInterpState :: BBID -> InterpState
mkInitInterpState entryid =  InterpState {
    prevbbid  = Nothing,
    curbbid = entryid,
    env = empty
}



-- Interpret an expression
interpexp :: Expr -> State InterpState Int
interpexp (EConst i) = return i
interpexp (EIdent ident) = do
    e <- get env
    return (e ! ident)
interpexp (EAdd e1 e2) = liftA2 (+) (interpexp e1) (interpexp e2)

-- Interpret an assignment
interpassign :: Assign -> State InterpState ()
interpassign (Assign ident e) = do
    v <- interpexp e
    modify (\env -> insert ident v env)
    



-- Interpret a teriminator instruction to return the next BBID.
-- Values executing a terminator can take 
data TermValue = TVExit Int | TVBranch BBID
interpterm :: Terminator -> State InterpState TermValue
interpterm (Branch bbid) = return bbid
interpterm (BranchCond e idtrue idfalse) = do
    i <- interpexp e
    if i == 1 
    then return $ TVBranch idtrue
    else if i == 2
    then return $ TVBranch idfalse
    else error $ "undefined branching on :" ++ i

interpbb :: BB -> State InterpState TermValue
interpbb bb = do
    -- todo: interpphi
    forM_ (bbassigns bb) interpassign
    interpterm (bbterm bb) 
    
    

-- Returns the exit code
interpgo :: Program -> BBID -> State InterpState Int
interpgo p bbid = do
        let bb = programGetBB p bbid
        termval <- interpbb bb
        case termval of
            TVExit exitval -> return exitval
            TVBranch bbid' -> interpgo p bbid'

-- Interpreter
interp :: Program -> (Int, Env Int)
interp (Program p@(entry:_)) = (ret, env interpstate)
    where (ret, interpstate) = runState init (interpgo p  (bbid entry))
          init = mkInitInterpState (bbid entry)
\end{code}



\begin{code}
main :: IO ()
main = putStrLn "foo"
\end{code}
