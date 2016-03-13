{-# LANGUAGE OverloadedStrings #-}
-- import Toolbox
import Haste
import Haste.DOM
import Haste.Events
import Control.Monad

main = do
  withElems ["log", "alpha", "beta", "eta", "lambda-expression"] $
    \[log, a, b, e, lamex] -> do
      lamex `onEvent` KeyDown $ \k -> when (k == 13) $ do
        Just v <- getValue lamex
        logEntry <- newElem "li" `with` ["textContent" =: v]
        logEntry `onEvent` Click $ \e -> do
          putStrLn "Setting value..."
          setProp lamex "value" v
            
        log `appendChild` logEntry
          
      a `onEvent` Click $ \e -> putStrLn "Performing alpha..."
      b `onEvent` Click $ \e -> putStrLn "Performing beta..."
      e `onEvent` Click $ \e -> putStrLn "Performing eta..."
    

  
