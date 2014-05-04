module Primitives (primitiveBindings) where
import Control.Monad (liftM)
import Datatypes
import Eval (bindVars)

import Primitives.Equal
import Primitives.IO
import Primitives.List

import Unpackers

--Primitive environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

--Primitives:
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-append", strAppend),
			        ("char-list", strToChars),
              ("list-index", listIndex),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

strAppend :: [LispVal] -> ThrowsError LispVal
strAppend [String s] = return $ String s
strAppend (String s:ss) = do
  rest <- strAppend ss
  case rest of
    String s' -> return $ String $ s ++ s'
    _ -> throwError $ TypeMismatch "string" rest
strAppend [badType] = throwError $ TypeMismatch "string" badType
strAppend badArgList = throwError $ NumArgs 1 badArgList

strToChars :: [LispVal] -> ThrowsError LispVal
strToChars [String s] = return $ List $ map Char s
strToChars [badType] = throwError $ TypeMismatch "string" badType
strToChars badArgList = throwError $ NumArgs 1 badArgList
