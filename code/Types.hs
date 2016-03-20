module Types where 

import Control.Monad.State.Lazy

data Lambda = Func String Lambda | Expr [Lambda] | Name String

-- | An equation is a list of Lambda expressions and conversions and
--    reductions to them. Each part of the trace contains a lambda
type Equation    = [(TraceSymbol, Lambda)]

data TraceSymbol = AlphaConversion String Lambda
                 | BetaConversion
                 | EtaConversion String Lambda
-- AlphaConversion (Old Name) (New Name)
-- BetaConversion  (Variable Changed) (Argument applied)
-- EtaConversion   (Function Replaced) (New expression)

-- | A trace is a State transformer over IO, with equation as its state
type Trace       = StateT Equation IO

-- | There's three possible outcomes of beta results; only Reduced is a value
--    that will continue the equation
data BetaResult = Reduced | Impossible | NeedsAlphaConversion
    deriving (Show, Eq)
             
-- | There's three possible reductions (and conversions) on a lambda expression
--    Alpha: change the name on the arguments
--    Beta: apply the function
--    Eta: make it pointfree, \x -> fun x == fun
data Reduction = Alpha | Beta | Eta | None
    deriving (Show, Eq)

addTrace :: TraceSymbol -> Lambda -> Trace ()
addTrace ts lam = modify (++ [(ts, lam)])

