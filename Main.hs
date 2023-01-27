module Main where

-- Imported modules
import Blocks
import BlockchainState

main :: IO Blockchain
main = do
    runner <- restartStateT [genesisBlock]
    runStateT runner $ do
        new <- extend (Transaction "Alice" "Bob" 10)
        mine
        newer <- extend (Transaction "Bob" "Charles" 100)
        mine
        getChain
