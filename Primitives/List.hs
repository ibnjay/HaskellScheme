module Primitives.List where

import Datatypes

listIndex :: [LispVal] -> ThrowsError LispVal
listIndex [Number n, List xs] = return $ xs !! fromInteger n
listIndex badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x :xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_  : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

listPrimitives =
    [ ("list-index", listIndex)
    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)
    ]
    