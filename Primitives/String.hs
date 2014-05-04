module Primitives.String where

import Data.List.Split (splitOn)
import Datatypes
import Primitives.Operators

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

strCons :: [LispVal] -> ThrowsError LispVal
strCons [Char c, String s] = return $ String (c : s)
strCons badArgList = throwError $ NumArgs 2 badArgList

strSplit :: [LispVal] -> ThrowsError LispVal
strSplit [String sep, String s] = return . List . map String $ splitOn sep s
strSplit badArgList = throwError $ NumArgs 2 badArgList

stringPrimitives = 
    [ ("string=?", strBoolBinop (==))
    , ("string<?", strBoolBinop (<))
    , ("string>?", strBoolBinop (>))
    , ("string<=?", strBoolBinop (<=))
    , ("string>=?", strBoolBinop (>=))
    , ("string-append", strAppend)
    , ("string-split", strSplit)
    , ("char-list", strToChars)
    , ("string-cons", strCons)
    ]
