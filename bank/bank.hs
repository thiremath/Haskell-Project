-- Name - Tejas Hiremath
-- Project 6

module Bank where

-- Define BankOp as a wrapper for a state transition function
newtype BankOp a = BankOp (Float -> (a, Float))

-- Functor instance for BankOp
instance Functor BankOp where
    fmap f (BankOp g) = BankOp (\state -> 
        let (result, newState) = g state 
        in (f result, newState))

-- Applicative instance for BankOp
instance Applicative BankOp where
    pure x = BankOp (\state -> (x, state))
    BankOp f <*> BankOp x = BankOp (\state -> 
        let (func, state') = f state
            (result, state'') = x state'
        in (func result, state''))

-- Monad instance for BankOp to enable chaining
instance Monad BankOp where
    return = pure
    BankOp action >>= next = BankOp (\state -> 
        let (result, state') = action state
            BankOp nextAction = next result
        in nextAction state')

-- Deposit: Add a specified amount to the balance
deposit :: Float -> BankOp ()
deposit amount = BankOp (\balance -> ((), balance + amount))

-- Withdraw: Subtract an amount, allowing up to $100 overdraft
withdraw :: Float -> BankOp Float
withdraw amount = BankOp (\balance -> 
    let actualWithdrawal = if balance - amount >= -100 
                           then amount 
                           else balance + 100
    in (actualWithdrawal, balance - actualWithdrawal))

-- GetBalance: Retrieve the current balance
getBalance :: BankOp Float
getBalance = BankOp (\balance -> (balance, balance))

-- Run a BankOp operation starting with an initial balance of 0
runBankOp :: BankOp a -> a
runBankOp (BankOp op) = fst (op 0)
