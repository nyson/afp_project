data Lambda = Func String Lambda | Expr [Lambda] | Name String

type Equation    = [(TraceSymbol, Lambda)]

data TraceSymbol = AlphaConversion (Name String) (Name String)
                 | BetaConversion (Name String) (Expr Lambda)
                 | EtaConversion (Name String) Lambda
-- AlphaConversion (Old Name) (New Name)
-- BetaConversion  (Variable Changed) (Argument applied)
-- EtaConversion   (Function Replaced) (New expression)

type Trace       = State Equation 
