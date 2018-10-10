import Data.Int
import qualified Data.Primitive.MutVar as Var
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as Vec
import Data.Bits
import Control.Monad

-- char sieve[100000000 >> 3];
-- 
-- int main()
-- {
--     int count = 0;
--     for(long int i = 2; i < 100000000; i++)
--         if(!(sieve[i >> 3] & (1 << (i & 7))))
--         {
--             count++;
--             for(long int j = i * i; j < 100000000; j += i)
--                 sieve[j >> 3] |= 1 << (j & 7);
--         }
--     printf("%ld\n", count);
-- }
--
-- 
--

(.>>.) :: Int -> Int -> Int
(.>>.) = shiftR

(.<<.) :: Int -> Int -> Int
(.<<.) = shiftL

size :: Int
size = 100000000
 
ixs :: [Int]
ixs = [2..size-1]

sieve :: PrimMonad m => m (Vec.MVector (PrimState m) Bool)
sieve =  Vec.unsafeNew (size .>>. 3)

count :: PrimMonad m => m (Var.MutVar (PrimState m) Int)
count = Var.newMutVar 0

intToBool :: Int -> Bool
intToBool i = testBit  i 0

sievefn :: PrimMonad m => m Int
sievefn = do
    s <- sieve
    c <- count
    
    forM_  ixs (\i -> do
        sv <- Vec.unsafeRead s (i .>>. 3)
        let cond = (fromEnum sv .&. (1 .<<. (i .&. 7))) == 0

        if (cond)
           then do
                  Var.modifyMutVar c (+ 1)
                  forM_ [i*i, i*i+i..size-1] (\j -> do
                      let ix = j .>>. 3
                      let mask = (1 .<<. (j .&. 7))
                        
                      Vec.unsafeModify s (\val -> val || intToBool mask) ix)
           else return ()
        return ())
    Var.readMutVar c




main :: IO ()
main = do 
    out <- sievefn
    print out
