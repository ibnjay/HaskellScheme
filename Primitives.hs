module Primitives (primitiveBindings) where
import Control.Arrow (second)
import Control.Monad (liftM)
import Datatypes
import Eval (bindVars)

import Primitives.Equal
import Primitives.IO
import Primitives.List
import Primitives.Operators
import Primitives.String

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>=
    (flip bindVars $
        (second IOFunc `map` ioPrimitives)
     ++ (second PrimitiveFunc `map` primitives))

numericPrimitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div)
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    ]

numBoolPrimitives =
    [ ("=", numBoolBinop (==))
    , ("<", numBoolBinop (<))
    , (">", numBoolBinop (>))
    , ("/=", numBoolBinop (/=))
    , (">=", numBoolBinop (>=))
    , ("<=", numBoolBinop (<=))
    ]

boolBoolPrimitives =
    [ ("&&", boolBoolBinop (&&))
    , ("||", boolBoolBinop (||))
    ]

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = concat
    [ numericPrimitives
    , numBoolPrimitives
    , boolBoolPrimitives
    , listPrimitives
    , equalPrimitives
    , stringPrimitives
    , [("number->string", \xs -> case xs of
        [Number n] -> return $ String (show n))]
    ]
