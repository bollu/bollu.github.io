import Data.Int
import qualified Data.Primitive.MutVar as Var
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as Vec
import Data.Bits
import Control.Monad


-- char sieve[size >> 3];
--
-- int main()
-- {
--     int count = 0;
--     for(long int i = 2; i < size; i++)
--         if(!(sieve[i >> 3] & (1 << (i & 7))))
--         {
--             count++;
--             for(long int j = i * i; j < size; j += i)
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

sieve :: PrimMonad m => m (Vec.MVector (PrimState m) Int8)
sieve =  Vec.replicate (size .>>. 3) 0

count :: PrimMonad m => m (Var.MutVar (PrimState m) Int)
count = Var.newMutVar 0

intToBool :: Int -> Bool
intToBool i = testBit  i 0

sievefn :: PrimMonad m => m Int
sievefn = do
    s <- sieve
    c <- count

    -- for(long int i = 2; i < size; i++)
    forM_  [2..size-1] (\i -> do
      --
        -- if(!(sieve[i >> 3] & (1 << (i & 7))))
        --sv <- Vec.read s (i .>>. 3)
        sv <- Vec.unsafeRead s (i .>>. 3)
        let cond =  sv .&. (fromIntegral (1 .<<. (i .&. 7)))
        if (cond == 0)
           then do
                  -- count++;
                  Var.modifyMutVar c (+ 1)
                  -- for(long int j = i * i; j < size; j += i)
                  forM_ [i*i, i*i+i..size-1] (\j -> do
                      -- sieve[j >> 3] |= 1 << (j & 7);
                      --Vec.modify s (.|. fromIntegral (1 .<<. (j .&. 7))) (j .>>. 3))
                      Vec.unsafeModify s (.|. fromIntegral (1 .<<. (j .&. 7))) (j .>>. 3))
           else return ())
    Var.readMutVar c




main :: IO ()
main = do
    out <- sievefn
    print out
