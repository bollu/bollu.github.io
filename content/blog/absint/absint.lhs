http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/10.ABSTRACT-INTERPRETATION.html

Let's study abstract interpretation on an SSA like language with no mutation.

\begin{code}
import Algebra.Lattice
import qualified Data.Map.Strict as M
import Control.Monad.State 
import Control.Applicative
import Data.Foldable
import Data.Traversable

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
data Phi = Phi Ident [(BBID, Expr)]
data BB = BB { bbid :: BBID, bbphis :: [Phi], bbassigns :: [Assign], bbterm :: Terminator }

-- first basic block in program is the entry block.
newtype Program = Program [BB]

-- get the BB corresponding to BBID in Program p
programGetBB :: Program -> BBID -> BB
programGetBB (Program (bb:bbs)) needle =  
    if bbid bb == needle
    then bb
    else programGetBB (Program bbs) needle

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
    env = M.empty
}



-- Interpret an expression
interpexp :: Expr -> State InterpState Int
interpexp (EConst i) = return i
interpexp (EIdent ident) = do
    e <- gets env
    return (e M.! ident)

interpexp (EAdd e1 e2) = liftA2 (+) (interpexp e1) (interpexp e2)

setIdent :: Ident -> Int -> State InterpState ()
setIdent ident v =  
    modify (\istate -> istate { env =  M.insert ident v (env istate) })

-- Phi nodes need to run with respect to an "old" interpreter state
-- while updating the new state.
interpphi :: InterpState -> Phi -> State InterpState ()
interpphi s (Phi ident invals) = do
    -- filter out all phi values that work 
    prev <- gets prevbbid

    let es = [e | (bbid, e) <- invals, Just bbid == prev]
    case es of
        [e] -> do
                let v  = evalState (interpexp e) s
                setIdent ident v
        _ -> error "too many or too few matches for phi node"

-- Interpret an assignment
interpassign :: Assign -> State InterpState ()
interpassign (Assign ident e) = do
    v <- interpexp e
    setIdent ident v
    



-- Interpret a teriminator instruction to return the next BBID.
-- Values executing a terminator can take 
data TermValue = TVExit Int | TVBranch BBID
interpterm :: Terminator -> State InterpState TermValue
interpterm (Branch bbid) = return (TVBranch bbid)
interpterm (BranchCond e idtrue idfalse) = do
    i <- interpexp e
    if i == 1 
    then return $ TVBranch idtrue
    else if i == 2
    then return $ TVBranch idfalse
    else error $ "undefined branching on :" ++ show i

interpbb :: BB -> State InterpState TermValue
interpbb bb = do
    -- get the current state for the phi nodes
    s <- get
    -- interpret the phi nodes with respect to the old state
    forM_ (bbphis bb) (interpphi s)
    -- interpret the body
    forM_ (bbassigns bb) interpassign

    -- interpret terminator
    interpterm (bbterm bb) 
    
    

-- Returns the exit code
sinterp :: Program -> BBID -> State InterpState Int
sinterp p bbid = do
        let bb = programGetBB p bbid
        termval <- interpbb bb
        case termval of
            TVExit exitval -> return exitval
            TVBranch bbid' -> sinterp p bbid'

-- Interpreter
runinterp :: Program -> (Int, Env Int)
runinterp (p@(Program (entry:_))) = (ret, env s)
    where (ret, s) = runState (sinterp p (bbid entry)) init
          init = mkInitInterpState (bbid entry)
\end{code}



\begin{code}
main :: IO ()
main = putStrLn "foo"
\end{code}
