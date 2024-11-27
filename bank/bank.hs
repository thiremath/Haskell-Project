module Bank where

newtype BankOp a = ???

deposit :: Float -> BankOp ()

withdraw :: Float -> BankOp Float

runBankOp :: BankOp a -> a



