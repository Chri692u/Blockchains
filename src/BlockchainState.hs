{-# LANGUAGE Rank2Types#-}

module BlockchainState where

-- Imported modules
import Blocks

-- Libraries
import qualified Control.Monad.State as S
import Data.IORef

type Blockchain = [Block]

newtype RunStateT s m = RunStateT {runStateT :: forall a. S.StateT s m a -> m a }

-- Start/Restart the state
restartStateT :: s -> IO (RunStateT s IO)
restartStateT init = do
    r <- newIORef init
    return $ RunStateT $ \f -> do
        s <- readIORef r
        (x,s') <- S.runStateT f s
        atomicModifyIORef' r $ const (s', x)

-- Get current head of the blockchain
getCurrent :: (Monad m) => S.StateT Blockchain m Blockchain
getCurrent = do
    blockchain <- S.get
    return $ [head blockchain]

-- Get hash value
getHash :: (Monad m) => String -> S.StateT Blockchain m Blockchain
getHash str = do
    blockchain <- S.get
    return $ filter (\block -> (hash block) == str) blockchain

-- Get entire blockchain
getChain :: (Monad m) => S.StateT Blockchain m Blockchain
getChain = S.get >>= return

showChain :: Blockchain -> String
showChain (b:bs) = "[ID:1, Transactions:" ++ Prelude.show b ++ "] -> " ++ showChain bs

-- Extend a blockchain with a transaction
extend :: (Monad m) => Transaction -> S.StateT Blockchain m Blockchain
extend t = do
    blockchain <- S.get
    let block' = [update (head blockchain) t]
    S.put $ block' ++ tail blockchain
    return block'
        where update (Block i ts h ph n) t' = (Block i (ts ++ [t']) h ph n)

--Mine at the block head
mine :: (Monad m) => S.StateT Blockchain m Blockchain
mine = do
    blockchain <- S.get
    let block' = mineBlock (head blockchain) 0
    let newHead = (Block (index block') [] "" (hash block') Nothing)
    S.put $ [newHead] ++ [block'] ++ tail blockchain
    return [block']