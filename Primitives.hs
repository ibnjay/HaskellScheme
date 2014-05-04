module Primitives (primitiveBindings) where
import Control.Monad (liftM)
import Datatypes
import Eval (bindVars)

import Primitives.Equal
import Primitives.IO
import Primitives.List
import Primitives.Operators
import Primitives.String

--Primitive environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)


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
    ]
