module Blocks where

import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.UTF8 (fromString, toString)

data Transaction = Transaction { from :: String
                               , to :: String
                               , amount :: Float
                               } deriving (Show)

data Block = Block { index :: Int
                   , transactions :: [Transaction]
                   , hash :: String
                   , prevHash :: String
                   , nonce :: Maybe Int
                   }

instance Show Block where
    show (Block i t h _ _) = "[ID:" ++ Prelude.show i ++ ", Transactions:" ++ (Prelude.show $ length t) ++ " Hash:" ++ h ++ "]\n"

-- An empty block constant
genesisBlock :: Block
genesisBlock = Block index transactions hash prevHash Nothing
    where index = 1
          transactions = []
          hash = ""
          prevHash = "0000000000000000000000000000000000000000000000000000000000000000"

-- Function to mine a block
mineBlock :: Block -> Int -> Block
mineBlock block@(Block i t _ ph _) n = case head encoding of
                                           '0' -> (Block i t block' ph (Just n))
                                           _ -> mineBlock block (n+1)
    where block' = hashBlock block
          ctx = SHA256.updates SHA256.init (fmap fromString [block', (Prelude.show n), ph])
          encoding = toString . Base16.encode $ SHA256.finalize ctx -- Basic idea

-- Function to encrypt a block
hashBlock :: Block -> String
hashBlock (Block i ts _ ph _) = toString $ Base16.encode digest
   where digest = SHA256.finalize ctx 
         ctx = SHA256.updates SHA256.init $ fmap fromString [blockString, ph]
         blockString = foldr ((++) . Prelude.show) "" ts