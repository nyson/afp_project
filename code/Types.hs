module Types where 

import Control.Monad.State.Lazy

data Lambda = Func String Lambda | Expr [Lambda] | Name String

type Equation    = [(TraceSymbol, Lambda)]

data TraceSymbol = AlphaConversion String Lambda
                 | BetaConversion String Lambda
                 | EtaConversion String Lambda
-- AlphaConversion (Old Name) (New Name)
-- BetaConversion  (Variable Changed) (Argument applied)
-- EtaConversion   (Function Replaced) (New expression)

type Trace       = StateT Equation IO


data BetaResult = Reduced | Impossible | NeedsAlphaConversion
    deriving (Show, Eq)


data Reduction = Alpha | Beta | Eta | None
    deriving (Show, Eq)

addTrace :: TraceSymbol -> Lambda -> Trace ()
addTrace ts lam = modify (++ [(ts, lam)])
