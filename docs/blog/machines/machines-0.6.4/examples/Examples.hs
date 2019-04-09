{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Examples where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Control.Exception
import Control.Monad.Trans
import Data.Machine
import Data.Machine.Group
import System.IO

-- this slurp slurps until an eof exception is raised.
slurpHandleBad :: Handle -> IO [String]
slurpHandleBad h = do
  s <- hGetLine h
  (s:) <$> slurpHandleBad h

-- this is the good slurp
-- it catches the exception, and cleans up.
slurpHandle :: Handle -> IO [String]
slurpHandle h = clean <$> slurp where
  clean = either (\(SomeException _) -> []) id
  slurp = try $ do { s <- hGetLine h; (s:) <$> slurpHandle h }

-- read a file, returning each line in a list
readLines :: FilePath -> IO [String]
readLines f = withFile f ReadMode slurpHandle

-- | bad slurping machine
crashes :: Handle -> MachineT IO k String
crashes h = repeatedly $ lift (hGetLine h) >>= yield

-- | here is a plan that yields all the lines at once.
slurpHandlePlan :: Handle -> PlanT k [String] IO ()
slurpHandlePlan h = lift (slurpHandle h) >>= yield

{-
 - but we want a plan that will yield one line at a time
 - until we are done reading the file
 - but before we can do that, we need a few helper combinators.
 -}

-- | getFileLines reads each line out of the given file and pumps them into the given process.
getFileLines :: FilePath -> ProcessT IO String a -> SourceT IO a
getFileLines path proc = src ~> proc where
  src :: SourceT IO String
  src = construct $ lift (openFile path ReadMode) >>= slurpLinesPlan
  slurpLinesPlan :: Handle -> PlanT k String IO ()
  slurpLinesPlan h = exhaust (clean <$> try (hGetLine h)) where
  clean = either (\(SomeException _) -> Nothing) Just

-- | lineCount counts the number of lines in a file
lineCount :: FilePath -> IO Int
lineCount path = head <$> (runT src) where
  src = getFileLines path (fold (\a _ -> a + 1) 0)

-- | run a machine and just take the first value out of it.
runHead :: (Functor f, Monad f) => MachineT f k b -> f b
runHead src = head <$> runT src

-- | lineCharCount counts the number of lines, and characters in a file
lineCharCount :: FilePath -> IO (Int, Int)
lineCharCount path = runHead src where
  src = getFileLines path (fold (\(l,c) s -> (l+1, c + length s)) (0,0))

-- | A Process that takes in a String and outputs all the words in that String
wordsProc :: Process String String
wordsProc = repeatedly $ do { s <- await; mapM_ yield (words s) }

-- | A Plan to print all input.
printPlan :: PlanT (Is String) () IO ()
printPlan = await >>= lift . putStrLn >> yield ()

-- | A Process that prints all its input.
printProcess :: ProcessT IO String ()
printProcess = repeatedly printPlan

-- | A machine that prints all the lines in a file.
printLines :: FilePath -> IO ()
printLines path = runT_ $ getFileLines path printProcess

-- | A machine that prints all the words in a file.
printWords :: FilePath -> IO ()
printWords path = runT_ $ getFileLines path (wordsProc ~> printProcess)

-- | A machine that prints all the lines in a file with the line numbers.
printLinesWithLineNumbers :: FilePath -> IO ()
printLinesWithLineNumbers path = runT_ (t ~> printProcess) where
  t :: TeeT IO Int String String
  t = tee (source [1..]) (getFileLines path echo) lineNumsT
  lineNumsT :: MachineT IO (T Integer String) String
  lineNumsT = repeatedly $ zipWithT $ \i s -> show i ++ ": " ++ s

uniq :: Bool
uniq = run (supply xs uniqMachine) == [1,2,3] where
  -- | Unix's "uniq" command using groupingOn
  -- (==)  means "groups are contiguous values"
  -- final means "run the 'final' machine over each group"
  uniqMachine :: (Monad m, Eq a) => ProcessT m a a
  uniqMachine = groupingOn (==) final

  xs :: [Int]
  xs = [1,2,2,3,3,3]

{-
def lineWordCount(fileName: String) =
  getFileLines(new File(fileName),
    (id split words) outmap (_.fold(_ => (1, 0), _ => (0, 1)))) execute

lineWordCount FilePath -> IO (Int, Int)
lineWordCount path = runHead lineWordCountSrc where
  lineWordCountSrc = echo
-}

